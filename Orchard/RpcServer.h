#ifndef RPCSERVER_H
#define RPCSERVER_H

#include <qxmlrpc/server.h>

#include <QObject>
#include <QSharedPointer>

class RpcServer : public QObject
{
    Q_OBJECT

private:
    int _port;
    QSharedPointer <xmlrpc::Server> _server;

public:
    explicit RpcServer (int port, QObject *parent = 0);
    virtual ~RpcServer ();

private slots:
    void processRequest (int requestId, QString methodName,
                         QList <xmlrpc::Variant> params);

};

#endif // RPCSERVER_H
