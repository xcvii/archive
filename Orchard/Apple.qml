import Qt 4.7

Rectangle {
    width: 50
    height: 50

    color: "transparent"
    state: "invisible"

    function toggleState () {
        state = (state == "invisible") ? "ripe" : "invisible"
    }

    Image {
        id: appleImg
        source: "qrc:pics/apple1.png"
    }

    states: [
        State {
            name: "invisible"
            PropertyChanges {
                target: appleImg
                visible: false
            }
        },

        State {
            name: "ripe"
            PropertyChanges {
                target: appleImg
                visible: true
            }
        }
    ]

    transitions: [
        Transition {
            from: "invisible"
            to: "ripe"

            SequentialAnimation {
                PropertyAction {
                    target: appleImg
                    property: "visible"
                    value: true
                }

                PropertyAction { target: appleImg; property: "x"; value: 0 }
                PropertyAction { target: appleImg; property: "y"; value: 0 }
                PropertyAction {
                    target: appleImg
                    property: "rotation"
                    value: 0
                }

                NumberAnimation {
                    target: appleImg;
                    property: "scale";
                    from: 0;
                    to: 1;
                    duration: 20000
                    easing.type: Easing.OutCubic
                }
            }
        },

        Transition {
            from: "ripe"
            to: "invisible"

            SequentialAnimation {
                PropertyAnimation {
                    target: appleImg
                    property: "y"
                    from: 0
                    to: 150
                    duration: 300
                }

                ParallelAnimation {

                    PropertyAnimation {
                        target: appleImg
                        property: "x"
                        from: 0
                        to: 150
                        duration: 2000
                        easing.type: Easing.InQuad
                    }
                    PropertyAnimation {
                        target: appleImg
                        property: "y"
                        from: 150
                        to: 300
                        duration: 2000
                        easing.type: Easing.InQuad
                    }
                    RotationAnimation {
                        target: appleImg
                        property: "rotation"
                        from: 0
                        to: 1800
                        duration: 2000
                        easing.type: Easing.InQuad
                    }
                }

                PropertyAction {
                    target: appleImg
                    property: "visible"
                    value: false
                }
            }
        }
    ]
}
