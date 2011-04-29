import Qt 4.7

Rectangle {
    id: route

    property string pickerName: "picker"

    color: "transparent"
    state: "walk"

    Rectangle {
        id: picker

        width: 55; height: 100
        color: "blue"

        Text {
            text: "<" + route.pickerName + ">"
            horizontalAlignment: "AlignHCenter"
            width: parent.width
            y: (-25)
        }
    }

    SequentialAnimation {
        id: walkAnimation
        loops: Animation.Infinite

        NumberAnimation {
            target: picker; property: "x"
            to: route.width - picker.width
            duration: 25000
//            velocity: 50
//            easing.type: Easing.Linear
        }

        PauseAnimation { duration: 500 }

        NumberAnimation {
            target: picker; property: "x"
            to: 0
            duration: 25000

//            velocity: 50
//            easing.type: Easing.Linear
        }

        PauseAnimation { duration: 500 }
    }

    states: [
        State {
            name: "walk"
            PropertyChanges {
                target: walkAnimation
                running: true
            }
            PropertyChanges {
                target: picker
                color: "blue"
            }
        },
        State {
            name: "idle"
            PropertyChanges {
                target: walkAnimation
                running: false;
            }
            PropertyChanges {
                target: picker
                color: "yellow"
            }
        },
        State {
            name: "spec"
            PropertyChanges {
                target: walkAnimation
                running: false;
            }
            PropertyChanges {
                target: picker
                color: "orange"
            }
        }
    ]
}
