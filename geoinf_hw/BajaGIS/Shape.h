#ifndef BAJAGIS_SHAPE_H
#define BAJAGIS_SHAPE_H

#include <QGraphicsItem>
#include <QGraphicsPathItem>
#include <QGraphicsPolygonItem>

namespace BajaGIS {


enum ShapeType { Point, Polyline, Polygon };


class Shape
{
  public:
    virtual ~Shape () = 0;
    virtual ShapeType shapeType () const = 0;
}; // class Shape

typedef QSharedPointer <Shape> ShapePtr;


class PointShape
  : public QGraphicsItem
  , public Shape
{
  public:
    PointShape (qreal x, qreal y);
    ~PointShape ();

    inline ShapeType shapeType () const { return Point; }

    QRectF boundingRect () const;
    void paint (QPainter *painter, const QStyleOptionGraphicsItem *option,
        QWidget *widget = 0);
}; // class PointShape


class PolylineShape
  : public QGraphicsPathItem
  , public Shape
{
  public:
    PolylineShape (qreal x, qreal y);
    ~PolylineShape ();

    void addPoint (qreal x, qreal y);
    void close ();

    inline ShapeType shapeType () const { return Polyline; }
}; // class PolylineShape


class PolygonShape
  : public QGraphicsPolygonItem
  , public Shape
{
  public:
    PolygonShape (qreal x, qreal y);
    ~PolygonShape ();

    void addPoint (qreal x, qreal y);

    inline ShapeType shapeType () const { return Polygon; }
}; // class PolygonShape


} // namespace BajaGIS

#endif // BAJAGIS_SHAPE_H
