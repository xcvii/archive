
TEMPLATE += app

QT += gui declarative xml network

LIBS += -lqxmlrpc

SOURCES += \
    main.cc \
    MessageServer.cc \
    Scene.cc

OTHER_FILES += \
    Orchard.qml \
    Picker.qml

HEADERS += \
    MessageServer.h \
    Scene.h

RESOURCES += \
    UI.qrc
