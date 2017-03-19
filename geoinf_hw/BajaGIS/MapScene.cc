#include "MapScene.h"

BajaGIS::MapScene::MapScene (QRectF const &rect, QObject *parent)
  : QGraphicsScene (rect, parent)
{
  _mode = MoveMode;
  _shapeMode = Point;
  _currentShape = 0;
}
  

void
BajaGIS::MapScene::focusOutEvent (QFocusEvent *event)
{
  QGraphicsScene::focusOutEvent (event);

  endCurrentEdit ();
}

void
BajaGIS::MapScene::keyPressEvent (QKeyEvent *event)
{
  QGraphicsScene::keyPressEvent (event);

  switch (event->key ())
  {
    case Qt::Key_Escape:
      endCurrentEdit ();
      break;

    case Qt::Key_C:
      if (_currentShape && Polyline == _currentShape->shapeType ())
      {
        dynamic_cast <PolylineShape *> (_currentShape)->close ();
        endCurrentEdit ();
      }
      break;

    default:
      ;
  }
}

#include <QtDebug>
void
BajaGIS::MapScene::mouseMoveEvent (QGraphicsSceneMouseEvent *event)
{
  QGraphicsScene::mouseMoveEvent (event);
}

void
BajaGIS::MapScene::mousePressEvent (QGraphicsSceneMouseEvent *event)
{
  QPointF point = event->scenePos ();

  if (Qt::LeftButton == event->button ())
  {
    switch (_mode)
    {
      case InsertMode:
        _insert (point.x (), point.y ());
        break;

      case MoveMode:
        break;

      default:
        ;
    }
  }

  QGraphicsScene::mousePressEvent (event);
}

void
BajaGIS::MapScene::_insert (qreal x, qreal y)
{
  if (_currentShape)
  {
    switch (_currentShape->shapeType ())
    {
      case Polyline:
        dynamic_cast <PolylineShape *> (_currentShape)
            ->addPoint (x, y);
        break;

      case Polygon:
        dynamic_cast <PolygonShape *> (_currentShape)
            ->addPoint (x, y);
        break;

      default:
        ;
    }
  }
  else // !_currentShape
  {
    switch (_shapeMode)
    {
      case Point:
        addItem (new PointShape (x, y));

        break;
      case Polyline:
        _currentShape = new PolylineShape (x, y);
        addItem (dynamic_cast <PolylineShape *> (_currentShape));
        break;

      case Polygon:
        _currentShape = new PolygonShape (x, y);
        addItem (dynamic_cast <PolygonShape *> (_currentShape));
        break;

      default:
        ;
    }
  } // if (_currentShape)
}

