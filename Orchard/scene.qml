import Qt 4.7

Rectangle {
    id: scene

    width: 640
    height: 480

    color: "lightblue"

    Rectangle {
        id: field

        width: parent.width
        height: parent.height / 2
        anchors.bottom: parent.bottom

        color: "green"
    }

    Row {
        id: treeRow
        y: parent.height / 3
        spacing: (parent.width - trees.count * 100) / (trees.count - 1)

        Repeater {
            id:trees

            model: treeStates

            delegate: Tree {
                appleState: treeState
            }
        }
    }

    Repeater {
        model: pickerModel

        delegate: Picker {
            pickerName: name
            state: state

            width: scene.width
            y: 300
        }
    }
}
