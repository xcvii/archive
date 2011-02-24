#ifndef BAJAGIS_SHAPE_H
#define BAJAGIS_SHAPE_H

#include <QtGui/QGraphicsItem>

namespace BajaGIS {

enum Shape { Point, Polyline, Polygon };

class PointShape : public QGraphicsItem
{ };

class PolylineShape : public QGraphicsItem
{ };

class PolygonShape : public QGraphicsItem
{ };

template <Shape> struct shape
{ typedef PointShape type; };

template <> struct shape <Polyline>
{ typedef PolylineShape type; };

template <> struct shape <Polygon>
{ typedef PolygonShape type; };

}

#endif // BAJAGIS_SHAPES_H

