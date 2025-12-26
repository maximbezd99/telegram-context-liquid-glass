import UIKit
import Metal

private struct MorphShape: Hashable {
    let id: String
    var center: CGPoint
    var size: CGSize
    var cornerRadius: CGFloat
    var color: UIColor
    var highlight: Highlight

    struct Highlight: Hashable {
        var position: CGPoint
        var intensity: CGFloat
        var waveTime: CGFloat

        static let none = Highlight(position: .zero, intensity: 0, waveTime: -1)
    }

    var radii: CGSize {
        CGSize(width: size.width / 2, height: size.height / 2)
    }
}

private final class WeakRef<T: AnyObject> {
    weak var value: T?
    init(_ value: T) { self.value = value }
}

private final class MorphingRegistry {
    static let shared = MorphingRegistry()

    private var containers: [String: WeakRef<MorphingContainerView>] = [:]
    private var shapeViews: [String: [WeakRef<MorphingShapeView>]] = [:]

    private init() {}

    func registerContainer(_ container: MorphingContainerView, key: String) {
        containers[key] = WeakRef(container)
    }

    func unregisterContainer(key: String) {
        containers.removeValue(forKey: key)
    }

    func registerShape(_ shape: MorphingShapeView, key: String) {
        if shapeViews[key] == nil {
            shapeViews[key] = []
        }
        shapeViews[key]?.append(WeakRef(shape))
    }

    func unregisterShape(_ shape: MorphingShapeView, key: String) {
        shapeViews[key]?.removeAll { $0.value === shape || $0.value == nil }
    }

    func shapes(for key: String) -> [MorphingShapeView] {
        shapeViews[key]?.compactMap { $0.value } ?? []
    }

    func container(for key: String) -> MorphingContainerView? {
        containers[key]?.value
    }
}

public class MorphingShapeView: UIView, UIGestureRecognizerDelegate {
    public enum Corners: Equatable {
        case capsule
        case rounded(CGFloat)

        func radius(size: CGSize) -> CGFloat {
            switch self {
            case .capsule: min(size.width / 2, size.height / 2)
            case .rounded(let corner): min(size.width / 2, min(size.height / 2, corner))
            }
        }
    }

    let shapeId: String
    public let groupKey: String

    public var corners: Corners = .capsule
    public var shapeColor: UIColor = UIColor(white: 1, alpha: 1)
    public var interactionScale: CGFloat
    public var maxStretchScale: CGSize
    public var maxTranslation: CGSize

    private var currentHighlight: MorphShape.Highlight = .none
    private var highlightTimer: Timer?
    private var highlightAnimationStartTime: CFTimeInterval = 0

    private var stretchAnimationTimer: Timer?
    private var currentStretchDirection: CGVector = .zero
    private var currentStretchIntensity: CGFloat = 0
    private var baseStretchScale: CGFloat = 0
    private var stretchAnimationStartTime: CFTimeInterval = 0
    private var initialTouchPoint: CGPoint = .zero

    private var isRegistered = false

    private let stretchDistanceMultiplier: CGFloat = 5.0
    private let stretchAppearDuration: TimeInterval = 0.15
    private let stretchDecayDuration: TimeInterval = 0.25
    private let highlightAppearDuration: TimeInterval = 0.25
    private let highlightDisappearDuration: TimeInterval = 0.4

    public init(
        groupKey: String,
        interactionScale: CGFloat = 0.06,
        maxStretchScale: CGSize = CGSize(width: 0.1, height: 0.1),
        maxTranslation: CGSize = CGSize(width: 10, height: 10)
    ) {
        self.groupKey = groupKey
        self.shapeId = UUID().uuidString
        self.interactionScale = interactionScale
        self.maxStretchScale = maxStretchScale
        self.maxTranslation = maxTranslation
        super.init(frame: .zero)
        setup()
    }

    required init?(coder: NSCoder) { fatalError() }

    deinit {
        highlightTimer?.invalidate()
        stretchAnimationTimer?.invalidate()
        if isRegistered {
            MorphingRegistry.shared.unregisterShape(self, key: groupKey)
        }
    }

    private func setup() {
        backgroundColor = .clear
        isUserInteractionEnabled = true
        let longPress = UILongPressGestureRecognizer(target: self, action: #selector(handleGesture(_:)))
        longPress.minimumPressDuration = 0
        longPress.cancelsTouchesInView = false
        longPress.delaysTouchesBegan = false
        longPress.delaysTouchesEnded = false
        longPress.delegate = self
        addGestureRecognizer(longPress)
    }

    public override func didMoveToWindow() {
        super.didMoveToWindow()
        if window != nil && !isRegistered {
            isRegistered = true
            MorphingRegistry.shared.registerShape(self, key: groupKey)
        } else if window == nil && isRegistered {
            isRegistered = false
            MorphingRegistry.shared.unregisterShape(self, key: groupKey)
        }
    }

    @objc private func handleGesture(_ gesture: UILongPressGestureRecognizer) {
        let location = gesture.location(in: self)

        switch gesture.state {
        case .began:
            initialTouchPoint = location
            startHighlight(at: location)
            animateStretchStart()

        case .changed:
            updateHighlightPosition(location)
            updateStretchTransform(at: location)

        case .ended, .cancelled, .failed:
            endHighlight(at: location)
            animateStretchDecay()

        default:
            break
        }
    }

    public func gestureRecognizer(
        _ gestureRecognizer: UIGestureRecognizer,
        shouldRecognizeSimultaneouslyWith otherGestureRecognizer: UIGestureRecognizer
    ) -> Bool { true }

    fileprivate func readShapeData(relativeTo ref: UIView) -> MorphShape? {
        let view = self

        let c = view.convert(CGPoint(x: view.bounds.midX, y: view.bounds.midY), to: ref)
        let cornerPoints = [
            CGPoint(x: 0, y: 0),
            CGPoint(x: view.bounds.width, y: 0),
            CGPoint(x: 0, y: view.bounds.height),
            CGPoint(x: view.bounds.width, y: view.bounds.height)
        ].map { view.convert($0, to: ref) }

        let xs = cornerPoints.map { $0.x }
        let ys = cornerPoints.map { $0.y }
        let size = CGSize(
            width: (xs.max() ?? 0) - (xs.min() ?? 0),
            height: (ys.max() ?? 0) - (ys.min() ?? 0)
        )

        var hl = currentHighlight
        hl.position = view.convert(currentHighlight.position, to: ref)

        return MorphShape(
            id: shapeId,
            center: c,
            size: size,
            cornerRadius: corners.radius(size: size),
            color: shapeColor,
            highlight: hl
        )
    }

    private func updateStretchTransform(at location: CGPoint) {
        let dx = location.x - initialTouchPoint.x
        let dy = location.y - initialTouchPoint.y

        let maxDistanceX = bounds.width * stretchDistanceMultiplier
        let maxDistanceY = bounds.height * stretchDistanceMultiplier

        let normalizedX = maxDistanceX > 0 ? dx / maxDistanceX : 0
        let normalizedY = maxDistanceY > 0 ? dy / maxDistanceY : 0

        currentStretchIntensity = min(sqrt(normalizedX * normalizedX + normalizedY * normalizedY), 1.0)

        let dragDistance = sqrt(dx * dx + dy * dy)
        if dragDistance > 0.001 {
            currentStretchDirection = CGVector(dx: dx / dragDistance, dy: dy / dragDistance)
        } else {
            currentStretchDirection = .zero
        }

        applyStretchTransform()
    }

    private func applyStretchTransform() {
        guard baseStretchScale > 0.001 || currentStretchIntensity > 0.001 else {
            layer.transform = CATransform3DIdentity
            return
        }

        let uniformScale = 1.0 + interactionScale * baseStretchScale
        let dx = currentStretchDirection.dx
        let dy = currentStretchDirection.dy

        let stretchX = 1.0 + maxStretchScale.width * currentStretchIntensity * abs(dx)
        let stretchY = 1.0 + maxStretchScale.height * currentStretchIntensity * abs(dy)
        let compressX = 1.0 - maxStretchScale.width * currentStretchIntensity * abs(dy)
        let compressY = 1.0 - maxStretchScale.height * currentStretchIntensity * abs(dx)

        var t = CATransform3DIdentity
        t = CATransform3DScale(t, uniformScale, uniformScale, 1.0)
        t = CATransform3DScale(t, stretchX * compressX, stretchY * compressY, 1.0)
        t = CATransform3DTranslate(t, dx * maxTranslation.width * currentStretchIntensity,
                                      dy * maxTranslation.height * currentStretchIntensity, 0)
        layer.transform = t
    }

    private func animateStretchStart() {
        stretchAnimationTimer?.invalidate()
        let startScale = baseStretchScale
        stretchAnimationStartTime = CACurrentMediaTime()

        if startScale >= 0.99 {
            baseStretchScale = 1.0
            applyStretchTransform()
            return
        }

        stretchAnimationTimer = Timer.scheduledTimer(withTimeInterval: 1.0/60.0, repeats: true) { [weak self] timer in
            guard let self else { return timer.invalidate() }
            let elapsed = CACurrentMediaTime() - stretchAnimationStartTime
            let progress = min(elapsed / stretchAppearDuration, 1.0)
            baseStretchScale = startScale + (1.0 - startScale) * (1.0 - pow(1.0 - progress, 2.5))
            applyStretchTransform()
            if progress >= 1.0 {
                timer.invalidate()
                baseStretchScale = 1.0
            }
        }
    }

    private func animateStretchDecay() {
        stretchAnimationTimer?.invalidate()
        let startScale = baseStretchScale
        let startIntensity = currentStretchIntensity
        let startDirection = currentStretchDirection
        stretchAnimationStartTime = CACurrentMediaTime()

        stretchAnimationTimer = Timer.scheduledTimer(withTimeInterval: 1.0/60.0, repeats: true) { [weak self] timer in
            guard let self else { return timer.invalidate() }
            let elapsed = CACurrentMediaTime() - stretchAnimationStartTime
            let progress = min(elapsed / stretchDecayDuration, 1.0)
            let eased = 1.0 - pow(1.0 - progress, 3.0)

            baseStretchScale = startScale * (1.0 - eased)
            currentStretchIntensity = startIntensity * (1.0 - eased)
            currentStretchDirection = CGVector(dx: startDirection.dx * (1.0 - eased), dy: startDirection.dy * (1.0 - eased))
            applyStretchTransform()

            if progress >= 1.0 {
                timer.invalidate()
                baseStretchScale = 0
                currentStretchIntensity = 0
                currentStretchDirection = .zero
                layer.transform = CATransform3DIdentity
            }
        }
    }

    private func startHighlight(at location: CGPoint) {
        highlightTimer?.invalidate()
        let startIntensity = currentHighlight.intensity
        highlightAnimationStartTime = CACurrentMediaTime()

        currentHighlight = MorphShape.Highlight(
            position: clamp(location),
            intensity: startIntensity,
            waveTime: 0
        )

        if startIntensity >= 0.99 {
            currentHighlight.intensity = 1.0
            return
        }

        highlightTimer = Timer.scheduledTimer(withTimeInterval: 1.0/60.0, repeats: true) { [weak self] timer in
            guard let self else { return timer.invalidate() }
            let elapsed = CACurrentMediaTime() - highlightAnimationStartTime
            let progress = min(elapsed / highlightAppearDuration, 1.0)
            let eased = 1.0 - pow(1.0 - progress, 2.5)
            currentHighlight.intensity = startIntensity + (1.0 - startIntensity) * eased
            currentHighlight.waveTime = eased
            if progress >= 1.0 {
                timer.invalidate()
                currentHighlight.intensity = 1.0
                currentHighlight.waveTime = 1.0
            }
        }
    }

    private func updateHighlightPosition(_ location: CGPoint) {
        currentHighlight.position = clamp(location)
    }

    private func endHighlight(at location: CGPoint) {
        highlightTimer?.invalidate()
        currentHighlight.position = clamp(location)
        let startIntensity = currentHighlight.intensity
        let startWaveTime = currentHighlight.waveTime
        highlightAnimationStartTime = CACurrentMediaTime()

        highlightTimer = Timer.scheduledTimer(withTimeInterval: 1.0/60.0, repeats: true) { [weak self] timer in
            guard let self else { return timer.invalidate() }
            let elapsed = CACurrentMediaTime() - highlightAnimationStartTime
            let progress = min(elapsed / highlightDisappearDuration, 1.0)
            let eased = 1.0 - pow(1.0 - progress, 2.0)
            currentHighlight.intensity = startIntensity * (1.0 - eased)
            currentHighlight.waveTime = startWaveTime + (2.0 - startWaveTime) * eased
            if progress >= 1.0 {
                timer.invalidate()
                currentHighlight = .none
            }
        }
    }

    private func clamp(_ p: CGPoint) -> CGPoint {
        CGPoint(x: max(0, min(p.x, bounds.width)), y: max(0, min(p.y, bounds.height)))
    }
}

public class MorphingContainerView: UIView {
    private let metalLayer = CAMetalLayer()
    private let maskMetalLayer = CAMetalLayer()
    private let blurView = UIVisualEffectView()
    private let renderer = MorphingRenderer()
    private var displayLink: CADisplayLink?
    private let groupKey: String
    private var lastDataHash: Int = 0

    public var blurStyle: UIBlurEffect.Style = .systemThinMaterial {
        didSet { blurView.effect = UIBlurEffect(style: blurStyle) }
    }
    public var spacing: CGFloat = 20
    public var tintOpacity: CGFloat = 0.15
    public var highlightOpacity: CGFloat = 0.6

    public init(groupKey: String) {
        self.groupKey = groupKey
        super.init(frame: .zero)
        setup()
    }

    required init?(coder: NSCoder) { fatalError() }

    private func setup() {
        backgroundColor = .clear
        isOpaque = false

        let device = MTLCreateSystemDefaultDevice()

        maskMetalLayer.device = device
        maskMetalLayer.pixelFormat = .bgra8Unorm
        maskMetalLayer.isOpaque = false
        maskMetalLayer.contentsScale = UIScreen.main.scale

        metalLayer.device = device
        metalLayer.pixelFormat = .bgra8Unorm
        metalLayer.isOpaque = false
        metalLayer.contentsScale = UIScreen.main.scale
        layer.addSublayer(metalLayer)

        blurView.effect = UIBlurEffect(style: blurStyle)
        blurView.frame = bounds
        blurView.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        insertSubview(blurView, at: 0)
        maskMetalLayer.frame = blurView.bounds
        blurView.layer.mask = maskMetalLayer
    }

    public override func didMoveToWindow() {
        super.didMoveToWindow()
        if window != nil {
            MorphingRegistry.shared.registerContainer(self, key: groupKey)

            renderer.prepare()

            displayLink = CADisplayLink(target: self, selector: #selector(displayLinkFired))
            displayLink?.add(to: .main, forMode: .common)
        } else {
            MorphingRegistry.shared.unregisterContainer(key: groupKey)
            displayLink?.invalidate()
            displayLink = nil
        }
    }

    public override func layoutSubviews() {
        super.layoutSubviews()
        let scale = UIScreen.main.scale
        let drawableSize = CGSize(width: bounds.width * scale, height: bounds.height * scale)
        metalLayer.frame = bounds
        metalLayer.drawableSize = drawableSize
        maskMetalLayer.frame = bounds
        maskMetalLayer.drawableSize = drawableSize
        renderer.updateSize(drawableSize)
        blurView.frame = bounds
    }

    @objc private func displayLinkFired() {
        let shapeViews = MorphingRegistry.shared.shapes(for: groupKey)
        let shapes = shapeViews
            .filter { $0.isVisible(depth: 1) }
            .compactMap { $0.readShapeData(relativeTo: self) }

        guard !shapes.isEmpty else { return }

        let dataHash = computeHash(shapes)
        guard dataHash != lastDataHash else { return }

        let scale = UIScreen.main.scale
        let pixelShapes = shapes.map { shape -> MorphShape in
            var h = shape.highlight
            h.position = CGPoint(x: h.position.x * scale, y: h.position.y * scale)
            return MorphShape(
                id: shape.id,
                center: CGPoint(x: shape.center.x * scale, y: shape.center.y * scale),
                size: CGSize(width: shape.size.width * scale, height: shape.size.height * scale),
                cornerRadius: shape.cornerRadius * scale,
                color: shape.color,
                highlight: h
            )
        }

        guard
            let colorDrawable = metalLayer.nextDrawable(),
            let maskDrawable = maskMetalLayer.nextDrawable()
        else { return }

        let result = renderer.render(
            colorDrawable: colorDrawable,
            maskDrawable: maskDrawable,
            shapes: pixelShapes,
            smoothness: spacing * scale,
            edgeWidth: 0.5 * scale,
            tintOpacity: tintOpacity,
            highlightOpacity: highlightOpacity
        )

        if result {
            lastDataHash = dataHash
        }
    }

    private func computeHash(_ shapes: [MorphShape]) -> Int {
        var hasher = Hasher()
        for s in shapes {
            hasher.combine(s)
        }
        return hasher.finalize()
    }
}

private struct MorphingUniforms {
    var shapeCount: Float
    var smoothness: Float
    var edgeWidth: Float
    var time: Float
    var tintOpacity: Float
    var highlightOpacity: Float
    var renderMask: Float
    var padding: Float = 0
}

private final class BundleHelper: NSObject {}

private final class MorphingRenderer {
    private var device: MTLDevice?
    private var commandQueue: MTLCommandQueue?
    private var pipelineState: MTLRenderPipelineState?
    private var vertexBuffer: MTLBuffer?
    private var viewSizeBuffer: MTLBuffer?
    private var viewSize: CGSize = .zero
    private var prepared = false
    private let startTime = CACurrentMediaTime()

    func prepare() {
        guard !prepared, let device = MTLCreateSystemDefaultDevice(), let queue = device.makeCommandQueue() else { return }
        prepared = true
        self.device = device
        self.commandQueue = queue

        let mainBundle = Bundle(for: BundleHelper.self)
        guard
            let path = mainBundle.path(forResource: "LiquidGlassComponentsBundle", ofType: "bundle"),
            let bundle = Bundle(path: path),
            let lib = try? device.makeDefaultLibrary(bundle: bundle),
            let vf = lib.makeFunction(name: "defaultVertex"),
            let ff = lib.makeFunction(name: "morphingFragment")
        else { return }

//        guard
//            let lib = device.makeDefaultLibrary(),
//            let vf = lib.makeFunction(name: "defaultVertex"),
//            let ff = lib.makeFunction(name: "morphingFragment")
//        else { return }

        let d = MTLRenderPipelineDescriptor()
        d.vertexFunction = vf
        d.fragmentFunction = ff
        d.colorAttachments[0].pixelFormat = .bgra8Unorm
        d.colorAttachments[0].isBlendingEnabled = true
        d.colorAttachments[0].rgbBlendOperation = .add
        d.colorAttachments[0].alphaBlendOperation = .add
        d.colorAttachments[0].sourceRGBBlendFactor = .sourceAlpha
        d.colorAttachments[0].sourceAlphaBlendFactor = .sourceAlpha
        d.colorAttachments[0].destinationRGBBlendFactor = .oneMinusSourceAlpha
        d.colorAttachments[0].destinationAlphaBlendFactor = .oneMinusSourceAlpha
        pipelineState = try? device.makeRenderPipelineState(descriptor: d)
    }

    func updateSize(_ size: CGSize) {
        guard let device, size != viewSize else { return }
        viewSize = size
        let verts: [Float] = [-1,-1,0,1, 1,-1,1,1, -1,1,0,0, 1,1,1,0]
        vertexBuffer = device.makeBuffer(bytes: verts, length: verts.count * 4, options: .storageModeShared)
        var sz: [Float] = [Float(size.width), Float(size.height)]
        viewSizeBuffer = device.makeBuffer(bytes: &sz, length: 8, options: .storageModeShared)
    }

    func render(
        colorDrawable: CAMetalDrawable,
        maskDrawable: CAMetalDrawable,
        shapes: [MorphShape],
        smoothness: CGFloat,
        edgeWidth: CGFloat,
        tintOpacity: CGFloat,
        highlightOpacity: CGFloat
    ) -> Bool {
        guard let device, let queue = commandQueue, let ps = pipelineState, let vb = vertexBuffer, let vsb = viewSizeBuffer else { return false }

        let time = Float(CACurrentMediaTime() - startTime)
        var data = [Float]()
        data.reserveCapacity(shapes.count * 12)
        for s in shapes {
            var r: CGFloat = 0, g: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
            s.color.getRed(&r, green: &g, blue: &b, alpha: &a)
            data += [
                Float(s.center.x), Float(s.center.y),
                Float(s.radii.width), Float(s.radii.height),
                Float(s.cornerRadius),
                Float(r), Float(g), Float(b),
                Float(s.highlight.position.x), Float(s.highlight.position.y),
                Float(s.highlight.intensity), Float(s.highlight.waveTime)
            ]
        }

        guard let shapeDataBuffer = device.makeBuffer(bytes: &data, length: data.count * 4, options: .storageModeShared) else { return false }

        var maskUniforms = MorphingUniforms(shapeCount: Float(shapes.count), smoothness: Float(smoothness), edgeWidth: Float(edgeWidth), time: time, tintOpacity: 1.0, highlightOpacity: 0.0, renderMask: 1.0)
        var fxUniforms = MorphingUniforms(shapeCount: Float(shapes.count), smoothness: Float(smoothness), edgeWidth: Float(edgeWidth), time: time, tintOpacity: Float(tintOpacity), highlightOpacity: Float(highlightOpacity), renderMask: 0.0)

        guard
            let maskUniformBuffer = device.makeBuffer(bytes: &maskUniforms, length: MemoryLayout<MorphingUniforms>.size, options: .storageModeShared),
            let fxUniformBuffer = device.makeBuffer(bytes: &fxUniforms, length: MemoryLayout<MorphingUniforms>.size, options: .storageModeShared),
            let cb = queue.makeCommandBuffer()
        else { return false }

        for (drawable, uniformBuffer) in [(maskDrawable, maskUniformBuffer), (colorDrawable, fxUniformBuffer)] {
            let rp = MTLRenderPassDescriptor()
            rp.colorAttachments[0].texture = drawable.texture
            rp.colorAttachments[0].loadAction = .clear
            rp.colorAttachments[0].storeAction = .store
            rp.colorAttachments[0].clearColor = MTLClearColor(red: 0, green: 0, blue: 0, alpha: 0)
            if let enc = cb.makeRenderCommandEncoder(descriptor: rp) {
                enc.setRenderPipelineState(ps)
                enc.setVertexBuffer(vb, offset: 0, index: 0)
                enc.setFragmentBuffer(uniformBuffer, offset: 0, index: 0)
                enc.setFragmentBuffer(shapeDataBuffer, offset: 0, index: 1)
                enc.setFragmentBuffer(vsb, offset: 0, index: 2)
                enc.drawPrimitives(type: .triangleStrip, vertexStart: 0, vertexCount: 4)
                enc.endEncoding()
            }
        }

        cb.present(maskDrawable)
        cb.present(colorDrawable)
        cb.commit()

        return true
    }
}

private extension UIView {
//    func isVisible(cache: inout [UIView: Bool]) -> Bool {
//        if let existing = cache[self] {
//            return existing
//        }
//
//        guard !isHidden, alpha > 0 else {
//            cache[self] = false
//            return false
//        }
//        guard frame.width > 0, frame.height > 0 else {
//            cache[self] = false
//            return false
//        }
//        guard window != nil || (self as? UIWindow != nil) else {
//            cache[self] = false
//            return false
//        }
//
//        if let superview {
//            return superview.isVisible(cache: &cache)
//        } else {
//            cache[self] = true
//            return true
//        }
//    }
    func isVisible(depth: Int) -> Bool {
        guard !isHidden, alpha > 0 else {
            return false
        }
        guard frame.width > 0, frame.height > 0 else {
            return false
        }
        guard window != nil || (self as? UIWindow != nil) else {
            return false
        }

        if let superview, depth > 0 {
            return superview.isVisible(depth: depth - 1)
        } else {
            return true
        }
    }
}
