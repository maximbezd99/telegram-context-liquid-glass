import SwiftUI
import Combine
import Foundation

private func easeOutQuad(_ t: CGFloat) -> CGFloat {
    return 1 - (1 - t) * (1 - t)  // Fast start, slow end
}

private func easeInQuad(_ t: CGFloat) -> CGFloat {
    return t * t  // Slow start, fast end
}

func cubicEaseInOutProgress(_ t: CGFloat, from: CGFloat, to: CGFloat) -> CGFloat {
    let clamped = clamp((t - from) / (to - from), min: 0, max: 1)
    if clamped < 0.5 {
        return 4 * clamped * clamped * clamped
    } else {
        return 1 - pow(-2 * clamped + 2, 3) / 2
    }
}

enum InteractionAnimation {
    struct Start {
        let begin: Date
    }
    struct Finish {
        let begin: Date
        let snapTargetUnitValue: CGFloat
        let snapStartValue: CGFloat
    }
    
    case start(Start)
    case stable
    case finish(Finish)
    
    var isFinishing: Bool {
        switch self {
        case .finish: return true
        default: return false
        }
    }
}

private final class Model: ObservableObject {
    struct Interaction {
        var startUnitValue: CGFloat
        var dragEndDate: Date?
        var currentAnimation: InteractionAnimation
        var lastTickUnitValue: CGFloat?
        var lastTickDate: Date?
        var signedUnitVelocity: CGFloat = 0
    }
    
    @Published private var updateTrigger: Int = 0
    @Published private var value: CGFloat
    
    var unitValue: CGFloat {
        getUnitValue(absoluteValue: value)
    }
    
    let padding: CGFloat = 16
    let range: ClosedRange<CGFloat>
    let step: CGFloat?
    let animationDuration: CGFloat = 0.2
    let velocityCap: CGFloat = 1.5
    private let onValueChanged: ((CGFloat) -> Void)?
    
    private(set) var interaction: Interaction?
    
    private var displayLink: CADisplayLink?
    private var cancellable: AnyCancellable?
    private var lastReturnedValue: CGFloat
    private let feedbackGenerator = UIImpactFeedbackGenerator(style: .light)
    
    init(
        value: CGFloat,
        range: ClosedRange<CGFloat>,
        step: CGFloat?,
        onValueChanged: ((CGFloat) -> Void)?
    ) {
        self.value = value
        self.lastReturnedValue = value
        self.range = range
        self.step = step
        self.onValueChanged = onValueChanged
    }
    
    deinit {
        stop()
    }
    
    func start() {
        guard displayLink == nil else { return }
        displayLink = CADisplayLink(target: self, selector: #selector(tick))
        displayLink?.add(to: .main, forMode: .common)
        
        cancellable = $value.dropFirst().sink { [weak self] newValue in
            guard let self, newValue != self.lastReturnedValue else { return }
            if step == nil {
                self.lastReturnedValue = newValue
                self.onValueChanged?(newValue)
                
                if newValue == self.range.lowerBound || newValue == self.range.upperBound {
                    feedbackGenerator.impactOccurred()
                }
            } else {
                let nearestValue = nearestStepAbsoluteValue(newValue)
                if newValue <= nearestValue, lastReturnedValue > nearestValue {
                    self.lastReturnedValue = nearestValue
                    self.onValueChanged?(nearestValue)
                    feedbackGenerator.impactOccurred()
                } else if newValue >= nearestValue, lastReturnedValue < nearestValue {
                    self.lastReturnedValue = nearestValue
                    self.onValueChanged?(nearestValue)
                    feedbackGenerator.impactOccurred()
                }
            }
        }
    }
    
    func stop() {
        displayLink?.invalidate()
        displayLink = nil
    }
    
    @objc private func tick(_ link: CADisplayLink) {
        guard var interaction else { return }
        let now = Date()
        
        updateTrigger += 1

        switch interaction.currentAnimation {
        case .start(let start):
            if now > start.begin + animationDuration {
                if let _ = interaction.dragEndDate {
                    interaction.currentAnimation = .finish(.init(
                        begin: now,
                        snapTargetUnitValue: nearestStepUnitValue(unitValue),
                        snapStartValue: unitValue
                    ))
                } else {
                    interaction.currentAnimation = .stable
                }
            }
            self.interaction = interaction
        case .stable:
            if let _ = interaction.dragEndDate {
                interaction.currentAnimation = .finish(.init(
                    begin: now,
                    snapTargetUnitValue: nearestStepUnitValue(unitValue),
                    snapStartValue: unitValue
                ))
            }
            self.interaction = interaction
        case .finish(let finish):
            let linearProgress = min(1, now.timeIntervalSince(finish.begin) / animationDuration)
            let curveProgress = easeOutQuad(linearProgress)
            let interpolatedUnit = mix(x: finish.snapStartValue, y: finish.snapTargetUnitValue, a: curveProgress)
            value = absoluteValue(unitValue: interpolatedUnit)
            if now > finish.begin + animationDuration + 1 {
                self.interaction = nil
            }
        }
        
        guard var interaction = self.interaction else { return }
        
        let dt = interaction.lastTickDate.map { now.timeIntervalSince($0) } ?? 0
        let velocityDecay: CGFloat = 4.0
        interaction.signedUnitVelocity *= 1 - min(1, velocityDecay * dt)
        if abs(interaction.signedUnitVelocity) <= 0.001 {
            interaction.signedUnitVelocity = 0
        }
        interaction.lastTickUnitValue = unitValue
        interaction.lastTickDate = now
        self.interaction = interaction
    }
    
    func dragChanged(value: DragGesture.Value, proxy: GeometryProxy) {
        let now = Date()
        
        var interaction: Interaction
        if var int = self.interaction {
            if case .finish(let finish) = int.currentAnimation {
                int.currentAnimation = .start(.init(begin: finish.begin))
                int.dragEndDate = nil
                int.startUnitValue = unitValue
                self.interaction = int
            }
            interaction = int
        } else {
            let canStart = abs(value.startLocation.x - uiThumbValue(proxy: proxy)) < 30
            guard canStart else { return }
            
            feedbackGenerator.prepare()
            
            let int = Interaction(
                startUnitValue: unitValue,
                currentAnimation: .start(.init(begin: now))
            )
            self.interaction = int
            interaction = int
        }
        
        let startLocation = uiThumbValue(unitValue: interaction.startUnitValue, proxy: proxy)
        
        let fullWidth = proxy.size.width - padding * 2
        let location = startLocation + value.translation.width - padding
        let unitValue = clamp((location / fullWidth), min: 0, max: 1)
        self.value = absoluteValue(unitValue: unitValue)

        if
            let lastUnitValue = self.interaction?.lastTickUnitValue,
            let lastTime = self.interaction?.lastTickDate
        {
            let dt = now.timeIntervalSince(lastTime)
            if dt > 0.001 {
                let rawVelocity = (unitValue - lastUnitValue) / dt
                let newRawVelocity = mix(x: interaction.signedUnitVelocity, y: rawVelocity, a: 0.3)
                if abs(newRawVelocity) > 0.0001 {
                    let velocitySign = newRawVelocity / abs(newRawVelocity)
                    interaction.signedUnitVelocity = velocitySign * min(
                        velocityCap,
                        abs(newRawVelocity)
                    )
                } else {
                    interaction.signedUnitVelocity = 0
                }
            }
        }
        
        interaction.lastTickUnitValue = unitValue
        interaction.lastTickDate = now
        self.interaction = interaction
    }
    
    func dragEnded(value: DragGesture.Value) {
        guard var interaction else { return }
        let now = Date()

        interaction.dragEndDate = now

        self.interaction = interaction
    }
    
    func uiThumbValue(proxy: GeometryProxy) -> CGFloat {
        uiThumbValue(unitValue: unitValue, proxy: proxy)
    }
    
    func uiThumbValue(unitValue: CGFloat, proxy: GeometryProxy) -> CGFloat {
        padding + (proxy.size.width - padding * 2) * unitValue
    }
    
    func uiProgressValue(proxy: GeometryProxy) -> CGFloat {
        uiProgressValue(unitValue: unitValue, proxy: proxy)
    }
    
    func uiProgressValue(unitValue: CGFloat, proxy: GeometryProxy) -> CGFloat {
        let thumbValue = uiThumbValue(unitValue: unitValue, proxy: proxy)
        
        let diffEdge = padding / proxy.size.width
        
        if unitValue < diffEdge {
            let blend = unitValue / diffEdge
            return thumbValue - padding * (1 - blend)
        } else if unitValue > 1 - diffEdge {
            let blend = (unitValue - (1 - diffEdge)) / diffEdge
            return thumbValue + padding * blend
        } else {
            return thumbValue
        }
    }
    
    func animationProgress(proxy: GeometryProxy) -> CGFloat {
        guard let interaction else { return 0 }

        let now = Date()
        switch interaction.currentAnimation {
        case .start(let start):
            let linearProgress = min(1, (now.timeIntervalSince1970 - start.begin.timeIntervalSince1970) / animationDuration)
            let curveProgress = easeOutQuad(linearProgress)
            return curveProgress
        case .stable:
            return 1
        case .finish(let finish):
            let linearProgress = min(1, (now.timeIntervalSince1970 - finish.begin.timeIntervalSince1970) / animationDuration)
            let curveProgress = easeInQuad(linearProgress)
            return 1 - curveProgress
        }
    }
    
    func velocityBasedDeformation(proxy: GeometryProxy) -> CGSize {
        guard let interaction else { return .init(width: 1, height: 1) }
        let horizontalVelocity = abs(interaction.signedUnitVelocity)
        let progress = cubicEaseInOutProgress(horizontalVelocity, from: 0, to: velocityCap)
        return CGSize(
            width: 1 + 0.12 * progress,
            height: 1 - 0.12 * progress
        )
    }
    
    func thumbRadii(proxy: GeometryProxy) -> CGSize {
        let progress = animationProgress(proxy: proxy)
        let baseThumbRaddi = CGSize(width: 19, height: 12)
        let fullAnimatedThumbRaddi = CGSize(width: 27, height: 16)
        let thumbRaddii = CGSize(
            width: mix(x: baseThumbRaddi.width, y: fullAnimatedThumbRaddi.width, a: progress),
            height: mix(x: baseThumbRaddi.height, y: fullAnimatedThumbRaddi.height, a: progress)
        )
        let deformation = velocityBasedDeformation(proxy: proxy)
        
        let widthCapped = clamp(
            thumbRaddii.width * deformation.width,
            min: baseThumbRaddi.width * 0.8,
            max: fullAnimatedThumbRaddi.width * 1.2
        )
        let heightCapped = clamp(
            thumbRaddii.height * deformation.height,
            min: baseThumbRaddi.height * 0.8,
            max: fullAnimatedThumbRaddi.height * 1.2
        )
        
        let finalRadii = CGSize(
            width: widthCapped,
            height: heightCapped
        )
        return finalRadii
    }
    
    private func getUnitValue(absoluteValue: CGFloat) -> CGFloat {
        let value = (absoluteValue - range.lowerBound) / (range.upperBound - range.lowerBound)
        return clamp(value, min: 0, max: 1)
    }
    
    private func absoluteValue(unitValue: CGFloat) -> CGFloat {
        let value = range.lowerBound + (range.upperBound - range.lowerBound) * unitValue
        return clamp(value, min: range.lowerBound, max: range.upperBound)
    }

    private func nearestStepUnitValue(_ currentUnitValue: CGFloat) -> CGFloat {
        guard let step else { return currentUnitValue }
        let steps = Array(stride(from: range.lowerBound, through: range.upperBound, by: step))
        guard !steps.isEmpty else { return currentUnitValue }

        let currentAbsolute = absoluteValue(unitValue: currentUnitValue)
        let nearest = steps.min(by: { abs($0 - currentAbsolute) < abs($1 - currentAbsolute) })!
        return getUnitValue(absoluteValue: nearest)
    }
    
    private func nearestStepAbsoluteValue(_ value: CGFloat) -> CGFloat {
        guard let step else { return value }
        let steps = Array(stride(from: range.lowerBound, through: range.upperBound, by: step))
        guard !steps.isEmpty else { return value }
        let nearest = steps.min(by: { abs($0 - value) < abs($1 - value) })!
        return nearest
    }
}

struct SwiftUIGlassSlider<V>: View where V : BinaryFloatingPoint, V.Stride : BinaryFloatingPoint {
    @ObservedObject private var model: Model
    
    init(
        value: V,
        range: ClosedRange<V>,
        step: V.Stride? = nil,
        onValueChanged: ((V) -> Void)?
    ) {
        self.model = Model(
            value: CGFloat(value),
            range: ClosedRange(uncheckedBounds: (lower: CGFloat(range.lowerBound), upper: CGFloat(range.upperBound))),
            step: step.map { CGFloat($0) },
            onValueChanged: { onValueChanged?(V($0)) }
        )
    }
    
    var body: some View {
        GeometryReader { proxy in
            let progressValue = model.uiProgressValue(proxy: proxy)
            let thumbCenter = CGPoint(x: model.uiThumbValue(proxy: proxy), y: proxy.size.height / 2)
            let animationProgress = model.animationProgress(proxy: proxy)
            let thumbRadii = model.thumbRadii(proxy: proxy)
            
            Group {
                VStack(spacing: 0) {
                    Spacer()
                    ZStack {
                        Capsule()
                            .fill(Color.gray)
                            .frame(height: 6)
                        
                        HStack(spacing: 0) {
                            Capsule()
                                .fill(Color.blue)
                                .frame(
                                    width: min(proxy.size.width, max(0, progressValue)),
                                    height: 6
                                )
                            Spacer(minLength: 0)
                        }
                    }
                    Spacer()
                }
                .background {
                    if let step = model.step {
                        let array = Array(stride(from: model.range.lowerBound, through: model.range.upperBound, by: step))
                        ZStack {
                            ForEach(array, id: \.self) { element in
                                let stepUnitValue = (element - model.range.lowerBound) / (model.range.upperBound - model.range.lowerBound)
                                let xPosition = model.uiThumbValue(unitValue: stepUnitValue, proxy: proxy)
                                Circle()
                                    .fill(Color.black)
                                    .frame(width: 2.5, height: 2.5)
                                    .position(x: xPosition, y: proxy.size.height / 2 + 6)
                            }
                        }
                    }
                }
            }
            .compositingGroup()
            .visualEffect { effect, effectProxy in
                effect
                    .scaleEffect(
                        y: 1,
                        anchor: .top
                    )
            }
            .overlay {
                Color.black.opacity(0.0001)
                    .gesture(dragGesture(proxy: proxy))
            }
            .background {
                ZStack(alignment: .center) {
                    Capsule()
                        .fill(.white)
                        .shadow(
                            color: .black.opacity(0.1),
                            radius: 8,
                            y: mix(x: 0, y: 8, a: animationProgress)
                        )
                    
                    Capsule()
                      .fill(.white)
                      .blendMode(.destinationOut)
                }
                .frame(width: thumbRadii.width * 2, height: thumbRadii.height * 2)
                .position(x: thumbCenter.x, y: thumbCenter.y)
                .compositingGroup()
            }
            .visualEffect { effect, effectProxy in
                effect.layerEffect(
                    ShaderLibrary.sliderThumb(
                        .float2(thumbCenter),
                        .float2(thumbRadii),
                        .float(animationProgress)
                    ),
                    maxSampleOffset: .init(width: 50, height: 50)
                )
            }
        }
        .frame(height: 40)
        .onAppear {
            model.start()
        }
        .onDisappear {
            model.stop()
        }
    }
    
    private func dragGesture(proxy: GeometryProxy) -> some Gesture {
        DragGesture(minimumDistance: 0)
            .onChanged { model.dragChanged(value: $0, proxy: proxy) }
            .onEnded { model.dragEnded(value: $0) }
    }
}

struct SwiftUIGlassSliderSampleView: View {
    @State private var sliderValue: Double = 50
    @State private var systemValue0: Double = 1
    @State private var systemValue1: Double = 50
    
    var body: some View {
        ZStack {
            Color.white
            
            VStack {
                
                SwiftUIGlassSlider<CGFloat>(
                    value: 0,
                    range: 0...40,
                    step: 11,
                    onValueChanged: {
                        print($0)
                    }
                )
                .padding()
                
                SwiftUIGlassSlider<CGFloat>(
                    value: 0,
                    range: 0...2,
                    step: 1,
                    onValueChanged: {
                        print($0)
                    }
                )
                .padding()
                
                SwiftUIGlassSlider<CGFloat>(
                    value: 20,
                    range: 20...100,
                    onValueChanged: {
                        print($0)
                    }
                )
                .padding()
                
                SwiftUIGlassSlider<CGFloat>(
                    value: 0,
                    range: 0...40,
                    onValueChanged: {
                        print($0)
                    }
                )
                .padding()
            }
        }
        .padding()
        .padding()
    }
}

#Preview {
    SwiftUIGlassSliderSampleView()
}
