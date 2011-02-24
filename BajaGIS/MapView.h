#ifndef BAJAGIS_MAPVIEW_H
#define BAJAGIS_MAPVIEW_H

#include "Shapes.h"

#include <QtGui/QGraphicsView>
#include <QtGui/QMouseEvent>
#include <QtGui/QWheelEvent>

namespace BajaGIS {

class MapView : public QGraphicsView
{
  Q_OBJECT

  public:
    MapView (QGraphicsScene *scene)
      : QGraphicsView (scene)
    { _init (); }

  protected:
    void mousePressEvent (QMouseEvent *event);
    void wheelEvent (QWheelEvent *event);

  private:
    BajaGIS::Shape _drawingMode;
    void _init ();

}; // class MapView

}

#endif // BAJAGIS_MAPVIEW_H

