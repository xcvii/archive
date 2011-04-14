#include "MessageServer.h"

MessageServer::MessageServer (quint16 port, QObject *parent)
    : QObject (parent)
    , _server (new QTcpServer (this))
    , _disconnectedMap (new QSignalMapper (this))
    , _readyReadMap (new QSignalMapper (this))
    , _port (port)
    , _nextSocketId (0)
{
    _server->listen (QHostAddress::Any, _port);

    connect (_server, SIGNAL (newConnection ()),
             SLOT (handleNewConnection ()));

    connect (_disconnectedMap, SIGNAL (mapped (int)),
             SLOT (handleDisconnected (int)));

    connect (_readyReadMap, SIGNAL (mapped (int)),
             SLOT (handleReadyRead (int)));
}

void MessageServer::handleNewConnection ()
{
    while (_server->hasPendingConnections ())
    {

        QTcpSocket *socket = _server->nextPendingConnection ();

        uint id = _nextSocketId++;

        connect (socket, SIGNAL (disconnected ()),
                 _disconnectedMap, SLOT (map ()));
        _disconnectedMap->setMapping (socket, id);

        connect (socket, SIGNAL (readyRead ()),
                 _readyReadMap, SLOT (map ()));
        _readyReadMap->setMapping (socket, id);

        _sockets[id] =
                QSharedPointer <QTcpSocket> (socket, &QObject::deleteLater);
    }
}

void MessageServer::handleDisconnected (int socketId)
{
    _sockets.remove (socketId);
}

void MessageServer::handleReadyRead (int socketId)
{
    emit newMessage (socketId);
}
