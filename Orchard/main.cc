#include "MessageServer.h"
#include "Scene.h"

#include <QApplication>
#include <QDeclarativeContext>
#include <QDeclarativeEngine>
#include <QDeclarativeView>

int main (int argc, char *argv[])
{
    QApplication app (argc, argv);

    MessageServer s (8001);
    QDeclarativeView view;
    Scene scene;

    view.engine ()->rootContext ()->setContextObject (&scene);
    view.setSource (QString ("qrc:Orchard.qml"));
    view.show ();

    return app.exec ();
}
