import Qt 4.7

Rectangle {
    Text {
        text: "apple pickers"
        x: 320; y: 240;
        NumberAnimation on rotation {
            from: 0; to: 360;
            duration: 2000
            loops: Animation.Infinite
        }
    }

    width: 640
    height: 480
}
