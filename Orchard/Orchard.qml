import Qt 4.7

Rectangle {
    Text {
        visible: false;

        text: "apple pickers"
        x: 100; y: 120;
        color: "red"
        font { pointSize: 16; bold: true }

        NumberAnimation on rotation {
            easing.type: Easing.OutElastic
            from: 0; to: 360
            duration: 3000
            loops: Animation.Infinite
        }
    }

    width: 320
    height: 240
}
