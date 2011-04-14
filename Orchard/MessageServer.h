#ifndef MESSAGESERVER_H
#define MESSAGESERVER_H

#include <QMap>
#include <QObject>
#include <QSharedPointer>
#include <QSignalMapper>
#include <QTcpServer>
#include <QTcpSocket>

class MessageServer : public QObject
{
    Q_OBJECT

    QTcpServer *_server;
    QSignalMapper *_disconnectedMap;
    QSignalMapper *_readyReadMap;
    QMap <uint, QSharedPointer <QTcpSocket> > _sockets;

    quint16 _port;
    uint _nextSocketId;

public:
    MessageServer (quint16 port, QObject *parent = 0);

signals:
    void newMessage (int socketId);

private slots:
    void handleNewConnection ();
    void handleDisconnected (int socketId);
    void handleReadyRead (int socketId);

public slots:

};

#endif // MESSAGESERVER_H
