import UIKit
import Metal
import MetalKit

public struct GlassLensConfiguration {
    public var edgeInset: CGFloat = 4
    public var samplingPadding: CGFloat = 40
    public var minification: CGFloat = 0.15
    public var magnification: CGFloat = 0.12
    public var distortionEdgeVertical: CGFloat = 0.1
    public var distortionEdgeHorizontal: CGFloat = 0.2
    public var chromaticAberration: CGFloat = 2
    public var blurIntensity: CGFloat = 1
    public var tintedColorSource: UIColor = .gray
    public var tintedColorTarget: UIColor = .blue
    public var animationDuration: CFTimeInterval = 0.28
    public var snapAnimationDuration: CFTimeInterval = 0.24
}

struct GlassLensState {
    let config: GlassLensConfiguration
    let itemFrames: [CGRect]

    let shaderSize: CGSize
    let parentSize: CGSize
    let baseFrame: CGRect
    let fullProgressFrame: CGRect
    let deformation: CGSize
    let progress: CGFloat

    let currentFrame: CGRect
    let currentCornerRadius: CGFloat

    var currentRadii: CGSize {
        CGSize(width: currentFrame.width / 2, height: currentFrame.height / 2)
    }

    var minLensCenter: CGPoint {
        let firstItemFrame = itemFrames[0]
        return CGPoint(x: firstItemFrame.minX + firstItemFrame.width * 0.4, y: firstItemFrame.height / 2)
    }

    var maxLensCenter: CGPoint {
        let lastItemFrame = itemFrames[itemFrames.count - 1]
        return CGPoint(x: lastItemFrame.minX + lastItemFrame.width * 0.6, y: lastItemFrame.height / 2)
    }

    func scaled(scale: CGFloat) -> GlassLensState {
        return GlassLensState(
            config: config,
            itemFrames: itemFrames.map { $0.applying(.init(scaleX: scale, y: scale)) },
            shaderSize: shaderSize.applying(.init(scaleX: scale, y: scale)),
            parentSize: parentSize.applying(.init(scaleX: scale, y: scale)),
            baseFrame: baseFrame.applying(.init(scaleX: scale, y: scale)),
            fullProgressFrame: fullProgressFrame.applying(.init(scaleX: scale, y: scale)),
            deformation: deformation,
            progress: progress,
            currentFrame: currentFrame.applying(.init(scaleX: scale, y: scale)),
            currentCornerRadius: currentCornerRadius * scale
        )
    }

    static func uniformsBufferSize() -> Int {
        4 * GlassLensState(config: .init(), itemFrames: [], shaderSize: .zero, parentSize: .zero, baseFrame: .zero, fullProgressFrame: .zero, deformation: .zero, progress: .zero, currentFrame: .zero, currentCornerRadius: 0).foregroundUniforms().count
    }

    //        float2 lensCenter;
    //        float2 lensRadii;
    //        float2 viewSize;
    //        float progress;
    //        float velocityPenalty;
    //        float minification;
    //        float magnification;
    //        float distortionEdgeV;
    //        float distortionEdgeH;
    //        float aberration;
    //        float blurIntensity;
    //        float4 tintedColorSource;
    //        float4 tintedColorTarget;

    func backgroundUniforms() -> [Float] {
        let tintSource = premultipliedComponents(c: config.tintedColorSource)
        let tintTarget = premultipliedComponents(c: config.tintedColorTarget)
        return  [
            Float(currentFrame.center.x),
            Float(currentFrame.center.y),
            Float(currentRadii.width),
            Float(currentRadii.height),
            Float(shaderSize.width),
            Float(shaderSize.height),
            Float(progress),
            Float(0),
            Float(config.minification),
            Float(0),
            Float(0.1),
            Float(0.1),
            Float(0),
            Float(config.blurIntensity),
            Float(0), Float(0), // byte padding
            tintSource.r, tintSource.g, tintSource.b, tintSource.a,
            tintTarget.r, tintTarget.g, tintTarget.b, tintTarget.a
        ]
    }

    func foregroundUniforms() -> [Float] {
        let tintSource = premultipliedComponents(c: config.tintedColorSource)
        let tintTarget = premultipliedComponents(c: config.tintedColorTarget)
        return  [
            Float(currentFrame.center.x),
            Float(currentFrame.center.y),
            Float(currentRadii.width),
            Float(currentRadii.height),
            Float(shaderSize.width),
            Float(shaderSize.height),
            Float(progress),
            Float(0),
            Float(0),
            Float(0),
            Float(config.distortionEdgeVertical),
            Float(config.distortionEdgeHorizontal * 1.5),
            Float(config.chromaticAberration),
            Float(config.blurIntensity),
            Float(0), Float(0), // byte padding
            tintSource.r, tintSource.g, tintSource.b, tintSource.a,
            tintTarget.r, tintTarget.g, tintTarget.b, tintTarget.a
        ]
    }
}

public protocol GlassLensViewParent: NSObjectProtocol, UIView {
    func provideSnapshots(samplingOffset: CGFloat, backgroundScale: CGFloat, foregroundScale: CGFloat) -> (background: UIImage, foreground: UIImage)
    func contentViewForMasking() -> UIView
    func placeLensShadow(view: UIView)
}

public final class GlassLensView: UIView {
    public var configuration = GlassLensConfiguration() {
        didSet {
            guard let parent = self.parentView else { return }
            configure(parent: parent)
        }
    }

    public var numberOfItems: Int = 4 {
        didSet {
            guard let parent = self.parentView else { return }
            configure(parent: parent)
        }
    }

    public var onItemSelected: ((Int, CGPoint) -> Void)?

    public var currentlySelected: Int? {
        guard lensCenter != .zero else { return nil }
        return buildState().flatMap { state in
            state.itemFrames.firstIndex { rect in
                rect.contains(lensCenter)
            }
        }
    }

    public func selectItem(at index: Int, animated: Bool = true) {
        guard let state = buildState(), index >= 0, index < state.itemFrames.count else { return }

        let targetCenter = state.itemFrames[index].center

        if animated {
            snapFromCenter = animatedLensCenter()
            snapToCenter = targetCenter
            snapAnimationStartTime = CACurrentMediaTime()
            lensCenter = targetCenter
            previousRestingLensCenter = targetCenter
        } else {
            lensCenter = targetCenter
            previousRestingLensCenter = targetCenter
            velocitySamples = []
        }
    }

    private var previousRestingLensCenter: CGPoint = .zero
    private var lensCenter: CGPoint = .zero

    private let metalView: GlassLensMetalView
    private let parentMaskView: GlassLensMaskView
    private let selfMaskView: GlassLensMaskView
    private let lensShadow: UIView
    private weak var parentView: GlassLensViewParent?

    private var displayLink: CADisplayLink?

    public override init(frame: CGRect) {
        let subviewsFrame = CGRect(origin: CGPoint(), size: frame.size)
        metalView = GlassLensMetalView(frame: subviewsFrame)
        lensShadow = UIView(frame: subviewsFrame)
        parentMaskView = GlassLensMaskView(frame: subviewsFrame, isSelf: false)
        selfMaskView = GlassLensMaskView(frame: subviewsFrame, isSelf: true)

        super.init(frame: frame)

        backgroundColor = .clear
        isUserInteractionEnabled = false

        metalView.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        lensShadow.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        parentMaskView.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        selfMaskView.autoresizingMask = [.flexibleWidth, .flexibleHeight]

        lensShadow.backgroundColor = UIColor(white: 0.9, alpha: 1)
        lensShadow.isUserInteractionEnabled = false

        metalView.backgroundColor = .clear
        metalView.isOpaque = false
        metalView.layer.isOpaque = false
        metalView.isUserInteractionEnabled = false
        addSubview(metalView)

        let pan = UILongPressGestureRecognizer(target: self, action: #selector(handlePan(_:)))
        pan.minimumPressDuration = 0.0
        addGestureRecognizer(pan)

        startDisplayLink()
    }

    required init?(coder: NSCoder) {
        fatalError()
    }

    deinit {
        displayLink?.invalidate()
    }

    public override func layoutSubviews() {
        super.layoutSubviews()
        rerender(force: true)
    }

    private func startDisplayLink() {
        guard displayLink == nil else { return }
        displayLink = CADisplayLink(target: self, selector: #selector(tick(_:)))
        displayLink?.add(to: .main, forMode: .common)
    }

    @objc private func tick(_ link: CADisplayLink) {
        rerender(force: false)
    }

    private var parentKVOs = [NSKeyValueObservation]()
    public func configure<T: GlassLensViewParent>(parent: T) {
        parentView = parent
//        parentKVOs = []
//        parentKVOs = [
//            parent.observe(\.frame, options: [.initial, .old, .new]) { [weak self] obs, change in
//                guard let self else { return }
//                self.frame = obs.frame.insetBy(dx: -self.configuration.samplingPadding, dy: -self.configuration.samplingPadding)
//            },
//            parent.observe(\.transform, options: [.initial, .old, .new]) { [weak self] obs, change in
//                self?.transform = obs.transform
//            }
//        ]

//        let edgeInset = configuration.edgeInset
//        let restingSize = CGSize(
//            width: (bounds.width - edgeInset * 2) / CGFloat(numberOfItems),
//            height: bounds.height - edgeInset * 2
//        )
//        let baseFrame = CGRect(x: edgeInset, y: edgeInset, width: restingSize.width, height: restingSize.height)

//        previousRestingLensCenter = baseFrame.center
//        lensCenter = baseFrame.center
        parent.contentViewForMasking().mask = self.parentMaskView
//        captureParentSnapshot()
        parent.placeLensShadow(view: lensShadow)
        rerender(force: true)
    }

    public func captureParentSnapshot() {
        guard let parent = parentView else { return }
        guard parent.bounds.size.width > 0 && parent.bounds.size.height > 0 else { return }

        self.isHidden = true
        parent.contentViewForMasking().mask = nil

        let snapshots = parent.provideSnapshots(samplingOffset: configuration.samplingPadding, backgroundScale: 1, foregroundScale: 1 + configuration.magnification)

        metalView.setSnapshotImages(
            background: snapshots.background,
            foreground: snapshots.foreground
        )

        self.isHidden = false
        parent.contentViewForMasking().mask = parentMaskView
        rerender(force: true)
    }

    private var lastTickLensCenter: CGPoint?
    private var lastVelocitySampleTime: CFTimeInterval?
    private var velocitySamples: [CGFloat] = []
    private let maxVelocitySamples = 5
    private let maxDelayedVelocitySamples = 10

    private func weightedAverage(_ samples: [CGFloat]) -> CGFloat {
        guard !samples.isEmpty else { return 0 }
        var weightedSum: CGFloat = 0
        var totalWeight: CGFloat = 0
        for (index, sample) in samples.enumerated() {
            let weight = CGFloat(index + 1)
            weightedSum += sample * weight
            totalWeight += weight
        }
        let result = weightedSum / totalWeight
        return abs(result) < 0.01 ? 0 : result
    }

    private var currentVelocity: CGFloat {
        let recentSamples = velocitySamples.suffix(maxVelocitySamples)
        return weightedAverage(Array(recentSamples))
    }

    private var delayedVelocity: CGFloat {
        let olderSamples = velocitySamples.prefix(maxDelayedVelocitySamples)
        return weightedAverage(Array(olderSamples))
    }

    private var pseudoAcceleration: CGFloat {
        currentVelocity - delayedVelocity
    }

    private func rerender(force: Bool) {
        guard
            force || panIsActive || currentVelocity != 0 || pseudoAcceleration != 0,
            let parent = parentView
        else { return }

        let now = CACurrentMediaTime()
        let currentCenter = animatedLensCenter()

        let dt = lastVelocitySampleTime.map { now - $0 } ?? 0
        if let lastCenter = lastTickLensCenter, dt > 0.001 {
            let dx = currentCenter.x - lastCenter.x
            let rawVelocity = dx / dt

            velocitySamples.append(rawVelocity)
            if velocitySamples.count > maxVelocitySamples + maxDelayedVelocitySamples {
                velocitySamples.removeFirst()
            }
        }

        lastVelocitySampleTime = now
        lastTickLensCenter = currentCenter

        parentMaskView.frame = self.bounds

        guard let state = buildState() else { return }

        if let panStartLocation, let panCurrentLocation {
            let animationProgress = state.progress
            let translation = panCurrentLocation.applying(.init(translationX: -panStartLocation.x, y: -panStartLocation.y))
            let transformProgressX = translation.x == 0 ? 0 : translation.x / abs(translation.x) * easeOutQuad(smoothstep(edge0: 0, edge1: 600, x: abs(translation.x)))
            let transformProgressY = translation.y == 0 ? 0 : translation.y / abs(translation.y) * easeOutQuad(smoothstep(edge0: 0, edge1: 1000, x: abs(translation.y)))

            let transformTranslate = CGAffineTransform(
                translationX: animationProgress * 4 * transformProgressX,
                y: animationProgress * 6 * transformProgressY
            )

            let transformScale = CGAffineTransform(
                scaleX: 1 + animationProgress * 0.02 - animationProgress * 0.03 * abs(transformProgressY),
                y: 1 + animationProgress * 0.02 + animationProgress * 0.03 * abs(transformProgressY)
            )

            parent.transform = transformTranslate.concatenating(transformScale)
        } else {
            parent.transform = .identity
        }

        parentMaskView.update(state: state)
        selfMaskView.update(state: state)
        metalView.render(state: state)

        lensShadow.frame = parent.convert(state.currentFrame, from: self)
        lensShadow.layer.cornerRadius = state.currentCornerRadius

        let lensDiffToVEdges = max(0, state.parentSize.height - state.currentFrame.height)
        let shaderAlpha = smoothstep(edge0: state.config.edgeInset * 2, edge1: 0, x: lensDiffToVEdges)
        lensShadow.alpha = 1.0 - shaderAlpha
        metalView.alpha = shaderAlpha
    }

    @objc private func handlePan(_ gesture: UILongPressGestureRecognizer) {
        handleGesture(gesture)
    }

    private var panStartLocation: CGPoint?
    private var panCurrentLocation: CGPoint?
    private var panAnimationStartTime: CFTimeInterval?
    private var panAnimationEndTime: CFTimeInterval?
    private var panIsActive: Bool = false
    private var currentInteractionTransform: CGAffineTransform = .identity
    private var snapAnimationStartTime: CFTimeInterval?
    private var snapFromCenter: CGPoint?
    private var snapToCenter: CGPoint?

    public func handleGesture(_ gesture: UIGestureRecognizer) {
        if case .began = gesture.state {
            panAnimationStartTime = CACurrentMediaTime()
            panAnimationEndTime = nil
            panIsActive = true
            panStartLocation = gesture.location(in: self)

            if let state = buildState() {
                let touchLocation = gesture.location(in: self)
                let closestCenter = state.itemFrames
                    .map { $0.center }
                    .min(by: { distance($0, touchLocation) < distance($1, touchLocation) })

                if let closestCenter {
                    snapFromCenter = lensCenter
                    snapToCenter = closestCenter
                    snapAnimationStartTime = CACurrentMediaTime()
                    lensCenter = closestCenter
                    previousRestingLensCenter = closestCenter
                }
            }
        }

        panCurrentLocation = gesture.location(in: self)

        guard
            let state = buildState(),
            let _ = self.parentView
        else { return }

        switch gesture.state {
        case .changed:
            guard let panStartLocation else { return }
            let translation = gesture.location(in: self).applying(.init(translationX: -panStartLocation.x, y: -panStartLocation.y))
            let newCenterUncapped = CGPoint(
                x: previousRestingLensCenter.x + translation.x,
                y: previousRestingLensCenter.y + translation.y
            )

            let newCenterX = min(state.maxLensCenter.x, max(state.minLensCenter.x, newCenterUncapped.x))
            let newCenter = CGPoint(x: newCenterX, y: lensCenter.y)

            lensCenter = newCenter
            if snapAnimationStartTime != nil {
                snapToCenter = newCenter
            }
        case .ended, .cancelled:
            var selectedIndex = 0
            var closestDistance = CGFloat.greatestFiniteMagnitude

            for (index, frame) in state.itemFrames.enumerated() {
                let dist = distance(frame.center, lensCenter)
                if dist < closestDistance {
                    closestDistance = dist
                    selectedIndex = index
                    }
            }

            let closestCenter = state.itemFrames[selectedIndex].center

            if snapAnimationStartTime != nil {
                snapToCenter = closestCenter
            } else {
                snapFromCenter = animatedLensCenter()
                snapToCenter = closestCenter
                snapAnimationStartTime = CACurrentMediaTime()
            }

            lensCenter = closestCenter
            previousRestingLensCenter = closestCenter

            onItemSelected?(selectedIndex, lensCenter)

            let doubleTimeSinceStart = (panAnimationStartTime ?? 0) + configuration.animationDuration * 2
            let normalEndTime = CACurrentMediaTime() + configuration.animationDuration
            panAnimationEndTime = max(doubleTimeSinceStart, normalEndTime)

            panIsActive = false
        default:
            break
        }
    }

    private func animatedLensCenter() -> CGPoint {
        guard
            let snapStart = snapAnimationStartTime,
            let fromCenter = snapFromCenter,
            let toCenter = snapToCenter
        else {
            return lensCenter
        }

        let elapsed = CACurrentMediaTime() - snapStart
        let progress = min(1, elapsed / configuration.snapAnimationDuration)
        let easedProgress = easeOutQuad(progress)

        if progress >= 1 {
            snapAnimationStartTime = nil
            snapFromCenter = nil
            snapToCenter = nil
            lensCenter = toCenter
            return toCenter
        }

        return CGPoint(
            x: mix(x: fromCenter.x, y: toCenter.x, a: easedProgress),
            y: mix(x: fromCenter.y, y: toCenter.y, a: easedProgress)
        )
    }

    private func buildState() -> GlassLensState? {
        guard let parent = self.parentView, numberOfItems > 0 else { return nil }

        let edgeInset = configuration.edgeInset

        let restingSize = CGSize(
            width: (parent.frame.size.width - edgeInset * 2) / CGFloat(numberOfItems),
            height: parent.frame.size.height - edgeInset * 2
        )

        let fullProgressSize = CGSize(
            width: restingSize.width + edgeInset * 4,
            height: restingSize.height + edgeInset * 4
        )

        let progress = animationProgress()

        let progressBasedSize = CGSize(
            width: mix(x: restingSize.width, y: fullProgressSize.width, a: progress),
            height: mix(x: restingSize.height, y: fullProgressSize.height, a: progress)
        )

        let velocity = currentVelocity
        let acceleration = pseudoAcceleration

        let unsignedVelocity = 0.2 * smoothstep(edge0: 0, edge1: 1000, x: abs(velocity))
        let unsignedAcceleration = 0.15 * smoothstep(edge0: 0, edge1: 1500, x: abs(acceleration))
        let deformation = CGSize(
            width: 1 + unsignedVelocity - unsignedAcceleration,
            height: 1 - unsignedVelocity + unsignedAcceleration
        )

        let deformedSize = CGSize(
            width: progressBasedSize.width * deformation.width,
            height: progressBasedSize.height * deformation.height
        )

        let currentLensCenter = animatedLensCenter()

        let baseFrame = CGRect(center: currentLensCenter, size: restingSize)
        let fullProgressFrame = CGRect(center: currentLensCenter, size: fullProgressSize)
        let deformedFrame = CGRect(center: currentLensCenter, size: deformedSize)

        let currentCornerRadius = min(deformedSize.width / 2, deformedSize.height / 2)

        let itemFrames = (0..<numberOfItems).map { index in
            let parentCoordinatesRect = CGRect(
                x: edgeInset + restingSize.width * CGFloat(index),
                y: edgeInset,
                width: restingSize.width,
                height: restingSize.height
            )
            return self.convert(parentCoordinatesRect, from: parent)
        }

        let state = GlassLensState(
            config: configuration,
            itemFrames: itemFrames,
            shaderSize: self.bounds.size,
            parentSize: parent.bounds.size,
            baseFrame: baseFrame,
            fullProgressFrame: fullProgressFrame,
            deformation: deformation,
            progress: progress,
            currentFrame: deformedFrame,
            currentCornerRadius: currentCornerRadius
        )

        return state
    }

    func animationProgress() -> CGFloat {
        let now = CACurrentMediaTime()

        if let panAnimationEndTime {
            let timeTillFinish = max(0, panAnimationEndTime - now)
            if let panAnimationStartTime {
                let animationTimeSinceStart = max(0, now - panAnimationStartTime)
                if animationTimeSinceStart < configuration.animationDuration {
                    return easeOutQuad(smoothstep(edge0: 0, edge1: configuration.animationDuration, x: max(0, animationTimeSinceStart)))
                } else {
                    return easeInQuad(smoothstep(edge0: 0, edge1: configuration.animationDuration, x: timeTillFinish))
                }
            }
        } else if let panAnimationStartTime {
            return easeOutQuad(smoothstep(edge0: 0, edge1: configuration.animationDuration, x: max(0, now - panAnimationStartTime)))
        }

        return panIsActive ? 1 : 0
    }
}

final class GlassLensMaskView: UIView {
    private let isSelf: Bool
    private var glassLensState: GlassLensState?

    private var maskLayer: CAShapeLayer {
        layer as! CAShapeLayer
    }

    override class var layerClass: AnyClass {
        CAShapeLayer.self
    }

    private let fillLayer = CAShapeLayer()

    init(frame: CGRect, isSelf: Bool) {
        self.isSelf = isSelf
        super.init(frame: frame)
        setup()
    }

    required init?(coder: NSCoder) {
        fatalError()
    }

    func update(state: GlassLensState) {
        self.glassLensState = state
        updateMask()
    }

    private func setup() {
        backgroundColor = .clear
        isUserInteractionEnabled = false

        maskLayer.fillRule = .evenOdd
        maskLayer.fillColor = isSelf ? UIColor.green.cgColor : UIColor.red.cgColor

        if !isSelf {
            fillLayer.fillColor = UIColor.red.cgColor
            layer.addSublayer(fillLayer)
        }
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        updateMask()
    }

    private func updateMask() {
        guard let state = self.glassLensState else { return }

        let lensRect = state.currentFrame
            .offsetBy(dx: -state.config.samplingPadding, dy: -state.config.samplingPadding)
            .insetBy(dx: 1, dy: 1)
        let cornerRadius = state.currentCornerRadius - 1
        let lensPath = UIBezierPath(roundedRect: lensRect, cornerRadius: cornerRadius)

        if isSelf {
            maskLayer.path = lensPath.cgPath
            maskLayer.allowsEdgeAntialiasing = true
        } else {
            CATransaction.begin()
            CATransaction.setDisableActions(true)
            let path = UIBezierPath(rect: bounds)
            path.append(lensPath)
            maskLayer.path = path.cgPath

            fillLayer.path = lensPath.cgPath
            fillLayer.opacity = state.currentFrame.height < state.parentSize.height ? 1 : 0

            CATransaction.commit()
        }
    }
}

private final class BundleHelper: NSObject {}

final class GlassLensMetalView: UIView {

    private var device: MTLDevice?
    private var commandQueue: MTLCommandQueue?
    private var distortionPipeline: MTLRenderPipelineState?
    private var overlayPipeline: MTLRenderPipelineState?
    private var vertexBuffer: MTLBuffer?

    private var backgroundUniformsBuffer: MTLBuffer?
    private var foregroundUniformsBuffer: MTLBuffer?

    private var backgroundTexture: MTLTexture?
    private var foregroundTexture: MTLTexture?

    override class var layerClass: AnyClass {
        return CAMetalLayer.self
    }

    private var metalLayer: CAMetalLayer {
        layer as! CAMetalLayer
    }

    override init(frame: CGRect) {
        super.init(frame: frame)
        commonInit()
    }

    required init?(coder: NSCoder) {
        fatalError()
    }

    private func commonInit() {
        guard let device = MTLCreateSystemDefaultDevice() else { return }
        self.device = device

        metalLayer.device = device
        metalLayer.pixelFormat = .bgra8Unorm
        metalLayer.framebufferOnly = false
        metalLayer.isOpaque = false
        metalLayer.contentsScale = UIScreen.main.scale

        backgroundColor = .clear
        isOpaque = false

        commandQueue = device.makeCommandQueue()

        setupPipelines()
        setupBuffers()
    }

    override func layoutSubviews() {
        super.layoutSubviews()
        metalLayer.drawableSize = CGSize(
            width: bounds.width * metalLayer.contentsScale,
            height: bounds.height * metalLayer.contentsScale
        )
    }

    private func setupPipelines() {
        let mainBundle = Bundle(for: BundleHelper.self)
        guard
            let path = mainBundle.path(forResource: "LiquidGlassComponentsBundle", ofType: "bundle"),
            let bundle = Bundle(path: path),
            let library = try? device?.makeDefaultLibrary(bundle: bundle),
            let vertexFunc = library.makeFunction(name: "defaultVertex"),
            let distortionFunc = library.makeFunction(name: "lensDistortionFragment")
//            let overlayFunc = library.makeFunction(name: "lensOverlayFragment")
        else { return }

        func makePipeline(fragmentFunc: MTLFunction) -> MTLRenderPipelineState? {
            let desc = MTLRenderPipelineDescriptor()
            desc.vertexFunction = vertexFunc
            desc.fragmentFunction = fragmentFunc
            desc.colorAttachments[0].pixelFormat = metalLayer.pixelFormat
            desc.colorAttachments[0].isBlendingEnabled = true
            desc.colorAttachments[0].rgbBlendOperation = .add
            desc.colorAttachments[0].alphaBlendOperation = .add
            desc.colorAttachments[0].sourceRGBBlendFactor = .one
            desc.colorAttachments[0].destinationRGBBlendFactor = .oneMinusSourceAlpha
            desc.colorAttachments[0].sourceAlphaBlendFactor = .one
            desc.colorAttachments[0].destinationAlphaBlendFactor = .oneMinusSourceAlpha
            return try? device?.makeRenderPipelineState(descriptor: desc)
        }

        distortionPipeline = makePipeline(fragmentFunc: distortionFunc)
//        overlayPipeline = makePipeline(fragmentFunc: overlayFunc)
    }

    private func setupBuffers() {
        let vertices: [Float] = [
            -1,  1,  0, 0,
            -1, -1,  0, 1,
             1,  1,  1, 0,
             1, -1,  1, 1
        ]

        let uniformsSize = GlassLensState.uniformsBufferSize()

        vertexBuffer = device?.makeBuffer(bytes: vertices, length: vertices.count * MemoryLayout<Float>.size, options: .storageModeShared)
        backgroundUniformsBuffer = device?.makeBuffer(length: uniformsSize, options: .storageModeShared)
        foregroundUniformsBuffer = device?.makeBuffer(length: uniformsSize, options: .storageModeShared)
    }

    func setSnapshotImages(
        background: UIImage,
        foreground: UIImage
    ) {
        backgroundTexture = createTexture(from: background)
        foregroundTexture = createTexture(from: foreground)
    }

    private func createTexture(from image: UIImage) -> MTLTexture? {
        guard let device = self.device else { return nil }

        let size = image.size
        let screenScale = image.scale

        UIGraphicsBeginImageContextWithOptions(size, false, screenScale)
        defer { UIGraphicsEndImageContext() }

        image.draw(at: .zero)

        guard let normalizedImage = UIGraphicsGetImageFromCurrentImageContext(),
              let cgImage = normalizedImage.cgImage else {
            return nil
        }

        let loader = MTKTextureLoader(device: device)
        return try? loader.newTexture(cgImage: cgImage, options: [
            .textureUsage: NSNumber(value: MTLTextureUsage.shaderRead.rawValue),
            .textureStorageMode: NSNumber(value: MTLStorageMode.shared.rawValue),
            .SRGB: false
        ])
    }

    func render(state: GlassLensState) {
        guard
            let commandQueue = commandQueue,
            let distortionPipeline = distortionPipeline,
//            let overlayPipeline = overlayPipeline,
            let drawable = metalLayer.nextDrawable(),
            let vertexBuffer = vertexBuffer,
            let backgroundUniformsBuffer = backgroundUniformsBuffer,
            let foregroundUniformsBuffer = foregroundUniformsBuffer,
            let backgroundTexture = backgroundTexture,
            let foregroundTexture = foregroundTexture
        else { return }

        var bgUniforms = state.scaled(scale: metalLayer.contentsScale).backgroundUniforms()
        memcpy(backgroundUniformsBuffer.contents(), &bgUniforms, bgUniforms.count * MemoryLayout<Float>.size)
        var fgUniforms = state.scaled(scale: metalLayer.contentsScale).foregroundUniforms()
        memcpy(foregroundUniformsBuffer.contents(), &fgUniforms, fgUniforms.count * MemoryLayout<Float>.size)

        let renderPassDesc = MTLRenderPassDescriptor()
        renderPassDesc.colorAttachments[0].texture = drawable.texture
        renderPassDesc.colorAttachments[0].loadAction = .clear
        renderPassDesc.colorAttachments[0].storeAction = .store
        renderPassDesc.colorAttachments[0].clearColor = MTLClearColor(red: 0, green: 0, blue: 0, alpha: 0)

        guard
            let commandBuffer = commandQueue.makeCommandBuffer(),
            let encoder = commandBuffer.makeRenderCommandEncoder(descriptor: renderPassDesc)
        else { return }

        encoder.setVertexBuffer(vertexBuffer, offset: 0, index: 0)

        // Pass 1: Background
        encoder.setRenderPipelineState(distortionPipeline)
        encoder.setFragmentBuffer(backgroundUniformsBuffer, offset: 0, index: 0)
        encoder.setFragmentTexture(backgroundTexture, index: 0)
        encoder.drawPrimitives(type: .triangleStrip, vertexStart: 0, vertexCount: 4)

        // Pass 2: Foreground
        encoder.setFragmentBuffer(foregroundUniformsBuffer, offset: 0, index: 0)
        encoder.setFragmentTexture(foregroundTexture, index: 0)
        encoder.drawPrimitives(type: .triangleStrip, vertexStart: 0, vertexCount: 4)

        // Pass 3: Overlay
//        encoder.setRenderPipelineState(overlayPipeline)
//        encoder.drawPrimitives(type: .triangleStrip, vertexStart: 0, vertexCount: 4)

        encoder.endEncoding()
        commandBuffer.present(drawable)
        commandBuffer.commit()
    }
}

private func smoothstep<V>(edge0: V, edge1: V, x: V) -> V where V : BinaryFloatingPoint, V.Stride : BinaryFloatingPoint {
    let t = min(max((x - edge0) / (edge1 - edge0), 0), 1)
    return t * t * (3 - 2 * t)
}

private func premultipliedComponents(c: UIColor) -> (r: Float, g: Float, b: Float, a: Float) {
    var r: CGFloat = 0, g: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
    c.getRed(&r, green: &g, blue: &b, alpha: &a)
    return (Float(r * a), Float(g * a), Float(b * a), Float(a))
}

private extension CGRect {
    init(center: CGPoint, size: CGSize) {
        self.init(
            x: center.x - size.width / 2,
            y: center.y - size.height / 2,
            width: size.width,
            height: size.height
        )
    }

    var center: CGPoint {
        CGPoint(x: midX, y: midY)
    }
}

private func easeOutQuad(_ t: CGFloat) -> CGFloat {
    return 1 - (1 - t) * (1 - t)  // Fast start, slow end
}

private func easeInQuad(_ t: CGFloat) -> CGFloat {
    return t * t  // Slow start, fast end
}

private func distance(_ a: CGPoint, _ b: CGPoint) -> CGFloat {
    let dx = a.x - b.x
    let dy = a.y - b.y
    return sqrt(dx * dx + dy * dy)
}

private func mix<T>(x: T, y: T, a: T) -> T where T: BinaryFloatingPoint {
    (1 - a) * x + a * y
}
