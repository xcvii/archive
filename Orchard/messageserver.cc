#include "messageserver.h"

#include <QXmlStreamWriter>

MessageServer::MessageServer (int port, QObject *parent)
    : QObject (parent)
    , _server (new QTcpServer (this))
    , _readyReadMap (new QSignalMapper (this))
    , _disconnectedMap (new QSignalMapper (this))
    , _port (port)
    , _nextSocketId (0)
{
    _server->listen(QHostAddress::Any, _port);

    connect (_server, SIGNAL (newConnection ()),
             SLOT (handleNewConnection ()));

    connect (_disconnectedMap, SIGNAL (mapped (int)),
             SLOT (handleDisconnected (int)));

    connect (_readyReadMap, SIGNAL (mapped (int)),
             SLOT (handleReadyRead (int)));
}

bool
MessageServer::sendMessage (int socketId, QString const &messageTitle,
                            QString const &messageBody)
{
    QByteArray message;
    QXmlStreamWriter writer (&message);

    writer.writeStartDocument ();
    writer.writeStartElement ("message");
    writer.writeTextElement("title", messageTitle);
    writer.writeTextElement("body", messageBody);
    writer.writeEndElement ();
    writer.writeEndDocument ();

    return (0 < _sockets[socketId]->write(message));
}

bool
MessageServer::getMessage (int socketId, /*out*/ QString &messageTitle,
                           /*out*/ QString &messageBody)
{
    QByteArray const &message = _sockets[socketId]->readAll ();

    QXmlStreamReader reader (message);

    while (!reader.atEnd () && !reader.hasError ())
    {
        reader.readNext ();

        if (reader.tokenType () == QXmlStreamReader::StartElement)
        {
            if (reader.name () == "title")
            {
                reader.readNext ();
                if (reader.tokenType () == QXmlStreamReader::Characters)
                {
                    messageTitle = reader.text ().toString ();
                }
            }
            if (reader.name () == "body")
            {
                reader.readNext ();
                if (reader.tokenType () == QXmlStreamReader::Characters)
                {
                    messageBody = reader.text ().toString ();
                }
            }
        }

        if (reader.tokenType () == QXmlStreamReader::EndElement)
        {
            if (reader.name () == "message")
            {
                break;
            }
        }
    }

    if (reader.hasError ())
    {
        qDebug () << reader.errorString ();
        qDebug () << message;
        messageTitle = messageBody = "";
        return false;
    }

    return !messageTitle.isEmpty () && !messageBody.isNull ();
}

void
MessageServer::handleNewConnection ()
{
    while (_server->hasPendingConnections ())
    {
        QTcpSocket *socket = _server->nextPendingConnection ();

        int id = _nextSocketId++;

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

void
MessageServer::handleDisconnected (int socketId)
{
    _sockets.remove (socketId);
    emit clientDC (socketId);
}

void
MessageServer::handleReadyRead (int socketId)
{
    emit newMessage (socketId);
}
