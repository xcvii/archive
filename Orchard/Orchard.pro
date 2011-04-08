
TEMPLATE += app

QT += gui declarative xml network

LIBS += -lqxmlrpc

SOURCES += \
    main.cc \
    RpcServer.cc

OTHER_FILES += \
    Orchard.qml

HEADERS += \
    RpcServer.h
