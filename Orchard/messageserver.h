#ifndef MESSAGESERVER_H
#define MESSAGESERVER_H

#include <QObject>
#include <QTcpServer>
#include <QSignalMapper>
#include <QSharedPointer>
#include <QTcpSocket>

class MessageServer : public QObject
{
    Q_OBJECT

    QTcpServer *_server;
    QSignalMapper *_readyReadMap;
    QSignalMapper *_disconnectedMap;
    QMap <int, QSharedPointer<QTcpSocket> > _sockets;

    int _port;
    int _nextSocketId;

public:
    explicit MessageServer (int port, QObject *parent = 0);

    bool sendMessage (int socketId, QString const &messageTitle,
                      QString const &messageBody);

    bool getMessage (int socketId, /*out*/ QString &messageTitle,
                     /*out*/ QString &messageBody);

signals:
    void newMessage (int socketId);
    void clientDC (int socketId);

private slots:
    void handleNewConnection ();
    void handleDisconnected (int socketId);
    void handleReadyRead (int socketId);

public slots:

};

#endif // MESSAGESERVER_H
