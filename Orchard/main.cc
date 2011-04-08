#include "RpcServer.h"

#include <QApplication>
#include <QDeclarativeView>

int main (int argc, char *argv[])
{
    QApplication app (argc, argv);

    RpcServer s (8001);

    QDeclarativeView view;
    view.setSource (QUrl::fromLocalFile("../Orchard/Orchard.qml"));
    view.show ();

    return app.exec ();
}
