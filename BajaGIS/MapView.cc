#include "MapView.h"

BajaGIS::MapView::MapView (BajaGIS::MapScene *scene, QWidget *parent)
  : QGraphicsView (scene, parent)
{ }

void
BajaGIS::MapView::wheelEvent (QWheelEvent *event)
{
  if (Qt::ControlModifier & event->modifiers ())
  {
    if (0 < event->delta ())
    {
      scale (1.2, 1.2);
    }
    else if (0 > event->delta ())
    {
      scale (0.8, 0.8);
    }
  }
  else
  {
    QGraphicsView::wheelEvent (event);
  }
}

