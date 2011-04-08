#include "RpcServer.h"

RpcServer::RpcServer (int port, QObject *parent)
    : QObject (parent)
    , _port (port)
    , _server (new xmlrpc::Server (this))
{
    _server->registerMethod ("add", QVariant::Int, QVariant::Int, QVariant::Int);

    connect (_server.data (),
             SIGNAL (incomingRequest (int, QString, QList <xmlrpc::Variant>)),
             this,
             SLOT (processRequest(int, QString, QList <xmlrpc::Variant>)));

    if (_server->listen (_port))
    {
        qDebug () << "xmlrpc server running on port" << _port;
    }
    else
    {
        qDebug () << "xmlrpc server failed to start on port" << _port;
    }
}

RpcServer::~RpcServer ()
{
    qDebug () << "xmlrpc server shutting down on port" << _port;
}

void RpcServer::processRequest(int requestId, QString methodName,
                               QList<xmlrpc::Variant> params)
{
    Q_UNUSED (params)
    int x = params[0].toInt ();
    int y = params[1].toInt ();

    if (methodName == "add")
    {
        qDebug () << "request for adding" << x << "and" << y;
        _server->sendReturnValue (requestId, x + y);
    }
}
