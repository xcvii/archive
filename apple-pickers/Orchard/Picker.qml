import Qt 4.7

Rectangle {
    id: route

    property string pickerName: "picker"
    property int pickerPosition: picker.x + picker.width / 2

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
//        loops: Animation.Infinite
        running: true;
        onCompleted: { restart (); }

        SmoothedAnimation {
            target: picker; property: "x"
            to: route.width - picker.width
            velocity: 30
            maximumEasingTime: 0
        }

//        PauseAnimation { duration: 500 }

        SmoothedAnimation {
            target: picker; property: "x"
            to: 0
            velocity: 30
            maximumEasingTime: 0
        }

//        PauseAnimation { duration: 500 }
    }

    states: [
        State {
            name: "walk"
            PropertyChanges {
                target: walkAnimation
//                running: true
                paused: false;
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
//                running: false;
                paused: true;
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
//                running: false;
                paused: true;
            }
            PropertyChanges {
                target: picker
                color: "purple"
            }
        }
    ]
}
