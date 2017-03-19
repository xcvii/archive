import Qt 4.7

Rectangle {
    id: scene

    width: 640
    height: 480

    color: "lightblue"

    Repeater {
        id: pickerRepeater
        model: pickerModel

        delegate: Picker {
            z: 10
            pickerName: nameProp
            state: stateProp

            width: scene.width
            y: 300
        }
    }

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
        anchors.left: parent.left; anchors.right: parent.right
        spacing: (parent.width - trees.count * 100) / (trees.count - 1)

        Repeater {
            id:trees

            model: treeStates

            delegate: Tree {
                appleState: treeState
                onAppleFall: {
                    var appleX = 50 + index * (treeRow.spacing + 100);
                    for (var i = 0; i < pickerRepeater.count; ++i) {
                        var picker = pickerRepeater.parent.children[i+3];
                        if (Math.abs (appleX - picker.pickerPosition) < 35) {
                            appleHitsPicker (picker.pickerName);
                        }
                    }
                }
            }
        }
    }
}
