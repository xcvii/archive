import Qt 4.7

Rectangle {
    property string appleState: ""

    width: 100; height: 200

    color: "transparent"

    Image {
        source: "qrc:pics/tree1.png"
    }

    Apple {
        id: apple

        x: 23; y: 53
        state: appleState
    }
}
