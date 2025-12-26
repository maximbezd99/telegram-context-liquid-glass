//
//  SwiftUISwitcherView.swift
//  LiquidGlassPlayground
//
//  Created by Maxim Bezdenezhnykh on 20/12/2025.
//

import SwiftUI
import Combine

private func animatePosition(
    startTime: Date,
    endTime: Date,
    now: Date,
    startPosition: CGFloat,
    targetPosition: CGFloat,
    easing: ((CGFloat) -> CGFloat)? = nil
) -> CGFloat {
    let totalDuration = endTime.timeIntervalSince(startTime)
    guard totalDuration > 0 else { return targetPosition }

    let elapsed = now.timeIntervalSince(startTime)
    let rawT = CGFloat(max(0, min(1, elapsed / totalDuration)))
    let t = easing?(rawT) ?? rawT  // Apply easing if provided

    return startPosition + (targetPosition - startPosition) * t
}

private func easeInOutCubic(_ t: CGFloat) -> CGFloat {
    if t < 0.5 {
        return 4 * t * t * t  // Ease-in: slow start
    } else {
        return 1 - pow(-2 * t + 2, 3) / 2  // Ease-out: slow end
    }
}

private func easeOutQuad(_ t: CGFloat) -> CGFloat {
    return 1 - (1 - t) * (1 - t)  // Fast start, slow end
}

private func easeInQuad(_ t: CGFloat) -> CGFloat {
    return t * t  // Slow start, fast end
}

private final class Model: ObservableObject {
    @Published var updateTrigger: Int = 0
    @Published var xProgress: CGFloat = 0
    @Published var enabled: Bool
    private var cancellable: AnyCancellable?
    
    var interaction: Interaction?
    
    struct Interaction {
        var dragStartTime: Date
        var dragEndTime: Date? = nil
        var animationEndTime: Date? = nil
        var endingAnimationStartPosition: CGFloat? = nil
        var switchTriggered: Bool = false
        var currentState: Bool
        var targetState: Bool
    }
    
    private var displayLink: CADisplayLink?
    private lazy var feedback = UISelectionFeedbackGenerator()
    var onValueChanged: ((Bool) -> Void)?
    
    let coordinatesName = UUID().uuidString
    let animationDuration: CGFloat = 0.28
    
    init(enabled: Bool) {
        self.enabled = enabled
        xProgress = enabled ? 1 : 0
    }
    
    deinit {
        stop()
    }
    
    func start() {
        guard displayLink == nil else { return }
        displayLink = CADisplayLink(target: self, selector: #selector(tick))
        displayLink?.add(to: .main, forMode: .common)
        feedback.prepare()
        
        cancellable = $enabled.dropFirst().sink { [weak self] enabled in
            self?.onValueChanged?(enabled)
        }
    }
    
    func stop() {
        displayLink?.invalidate()
        displayLink = nil
        cancellable = nil
    }
    
    @objc private func tick(_ link: CADisplayLink) {
        guard let interaction else { return }
        updateTrigger += 1
        
        let now = Date()
        if let animationEndTime = interaction.animationEndTime {
            let targetProgress: CGFloat = interaction.targetState ? 1 : 0

            xProgress = animatePosition(
                startTime: animationEndTime - 2 * animationDuration,
                endTime: animationEndTime,
                now: now,
                startPosition: interaction.endingAnimationStartPosition ?? 0,
                targetPosition: targetProgress,
                easing: easeInOutCubic
            )
            
            if (now.timeIntervalSince1970 - animationEndTime.timeIntervalSince1970) > 1 {
                self.interaction = nil
            }
        }
    }
    
    func animationProgress() -> CGFloat {
        guard let interaction else { return 0 }

        let now = Date()

        if let animationEndTime = interaction.animationEndTime {
            let interval = animationEndTime.timeIntervalSince(now)

            // Phase 1: Appearing (0 to animationDuration remaining)
            if interval - animationDuration > 0 {
                let startInterval = 2 * animationDuration - interval
                let rawProgress = clamp(startInterval / animationDuration, min: 0, max: 1)
                return easeOutQuad(rawProgress)  // Apply ease-out: appear quickly
            }

            // Phase 2: Disappearing (animationDuration to 0 remaining)
            let rawProgress = clamp(interval / animationDuration, min: 0, max: 1)
            return easeInQuad(rawProgress)  // Apply ease-in: stay visible then disappear quickly
        } else {
            let interval = now.timeIntervalSince(interaction.dragStartTime)
            let rawProgress = clamp(interval / animationDuration, min: 0, max: 1)
            return easeOutQuad(rawProgress)  // During drag: appear quickly
        }
    }
    
    func dragChanged(value: DragGesture.Value, proxy: GeometryProxy) {
        let start: CGFloat = enabled ? 1 : 0
        let unitTranslation = value.translation.width / (proxy.size.width * 0.7)
        xProgress = max(-0.15, min(1.15, (start + unitTranslation)))
        
        if var interaction, interaction.dragEndTime == nil {
            if !interaction.currentState {
                if abs(xProgress - 1) < 0.2 {
                    interaction.currentState = true
                    interaction.targetState = true
                    feedback.selectionChanged()
                }
            } else {
                if abs(xProgress) < 0.2 {
                    interaction.currentState = false
                    interaction.targetState = false
                    feedback.selectionChanged()
                }
            }
            self.interaction = interaction
        } else {
            let interaction = Interaction(
                dragStartTime: Date(),
                currentState: enabled,
                targetState: !enabled
            )
            self.interaction = interaction
            feedback.prepare()
        }
    }
    
    func dragEnded(value: DragGesture.Value) {
        guard var interaction else { return }
        
        let dragEndTime = Date()
        interaction.dragEndTime = dragEndTime
        
        let animationEndTime = max(
            interaction.dragStartTime + 2 * animationDuration,
            dragEndTime.addingTimeInterval(animationDuration)
        )
        interaction.animationEndTime = animationEndTime
        interaction.endingAnimationStartPosition = xProgress
        if interaction.currentState != interaction.targetState {
            feedback.selectionChanged()
        }
        interaction.currentState = interaction.targetState
        
        self.interaction = interaction
        self.enabled = interaction.targetState
    }
}

struct SwiftUISwitcherView: View {
    @ObservedObject private var model: Model
    
    init(enabled: Bool, onValueChanged: ((Bool) -> Void)?) {
        self.model = Model(enabled: enabled)
        self.model.onValueChanged = onValueChanged
    }
    
    var body: some View {
        GeometryReader { proxy in
            let thumbSize = CGSize(
                width: proxy.size.width * 3/5,
                height: proxy.size.height - 4
            )
            
            let minX = 2 + thumbSize.width / 2
            let maxX = proxy.size.width - thumbSize.width / 2 - 2
            
            let baseLensRadii = CGSize(width: thumbSize.width / 2, height: thumbSize.height / 2)

            let progressDx = (maxX - minX) * model.xProgress

            let lensCenter = CGPoint(
                x: 2 + thumbSize.width / 2 + progressDx,
                y: proxy.size.height / 2
            )

            let animationProgress = model.animationProgress()
            
            let lensRadii = CGSize(
                width: baseLensRadii.width * (1 + 0.45 * animationProgress),
                height: baseLensRadii.height * (1 + 0.35 * animationProgress)
            )
            
            ZStack {
                let enabled = model.interaction?.currentState ?? model.enabled
                Capsule()
                    .fill(enabled ? Color.green : Color(white: 0.9, opacity: 1))
                    .animation(.spring(response: 0.3, dampingFraction: 0.7), value: enabled)
            }
            .visualEffect { effect, effectProxy in
                return effect.layerEffect(
                    ShaderLibrary.glassDistortUnderLens3(
                        .float2(lensCenter),
                        .float2(lensRadii),
                        .float(animationProgress)
                    ),
                    maxSampleOffset: .init(width: 50, height: 50)
                )
            }
            .coordinateSpace(name: model.coordinatesName)
            .gesture(dragGesture(proxy: proxy))
        }
        .frame(width: 64, height: 32)
        .onAppear {
            model.start()
        }
        .onDisappear {
            model.stop()
        }
    }
    
    private func dragGesture(proxy: GeometryProxy) -> some Gesture {
        DragGesture(minimumDistance: 0)
            .onChanged { value in
                model.dragChanged(value: value, proxy: proxy)
            }
            .onEnded { value in
                model.dragEnded(value: value)
            }
    }
}

//private func easeInOutProgress(startTime: Date, endTime: Date, now: Date) -> CGFloat {
//    let totalDuration = endTime.timeIntervalSince(startTime)
//    guard totalDuration > 0 else { return 1.0 }
//    
//    let elapsed = now.timeIntervalSince(startTime)
//    let t = max(0, min(1, elapsed / totalDuration))
//
//    if t < 0.5 {
//        return 4 * t * t * t
//    } else {
//        return 1 - pow(-2 * t + 2, 3) / 2
//    }
//}

struct SwiftUISwitcherViewSampleView: View {
    @State var isEnabled = false
    var body: some View {
        ZStack {
            Color.white
            
//            Image(.sample)
//                .resizable()
//                .ignoresSafeArea()
            
            SwiftUISwitcherView(enabled: isEnabled, onValueChanged: nil)
        }
    }
}

#Preview {
    SwiftUISwitcherViewSampleView()
}
