#include "messageserver.h"
#include "orchard.h"

#include <QApplication>
#include <QDeclarativeView>
#include <QDeclarativeEngine>
#include <QDeclarativeContext>

int main (int argc, char *argv[])
{
    QApplication app (argc, argv);

    MessageServer *server = new MessageServer (8001);
    Orchard *orchard = new Orchard (server);

    QObject::connect (server, SIGNAL (newMessage (int)),
                      orchard, SLOT (getMessage (int)));

    QObject::connect (server, SIGNAL (clientDC (int)),
                      orchard, SLOT (pickerDC (int)));

    QDeclarativeView *view = new QDeclarativeView;
    view->engine ()->rootContext ()->setContextObject (orchard);
    view->engine ()->rootContext ()->setContextProperty ("pickerModel",
                                                         orchard->pickerModel ());
    view->engine ()->addImageProvider ("sprites", orchard->getSpriteProvider ());
    view->setSource (QString ("qrc:scene.qml"));
    view->show ();

    return app.exec ();
}
