#include "MapView.h"

#include <QPointF>

void
BajaGIS::MapView::mousePressEvent (QMouseEvent *event)
{
  QPointF point;
  switch (event->button ())
  {
    case Qt::LeftButton:
      point = mapToScene (event->x (), event->y ());
      scene()->addEllipse (point.x () - 2, point.y () - 2, 4, 4);
      break;
    case Qt::RightButton:
    case Qt::MidButton:
    default:
      break;
  }
}

void
BajaGIS::MapView::wheelEvent (QWheelEvent *event)
{
  if (Qt::ControlModifier == event->modifiers ())
  {
    if (0 < event->delta ())
    {
      scale (1.2, 1.2);
    }
    else
    {
      scale (0.8, 0.8);
    }
  }
  else
  {
    QGraphicsView::wheelEvent (event);
  }
}

void
BajaGIS::MapView::_init ()
{
  setCursor (Qt::CrossCursor);

  _drawingMode = Point;
}

