#ifndef BAJAGIS_MAPVIEW_H
#define BAJAGIS_MAPVIEW_H

#include "MapScene.h"

#include <QGraphicsView>

namespace BajaGIS {

class MapView : public QGraphicsView
{
  Q_OBJECT

  public:
    MapView (MapScene *scene, QWidget *parent = 0);

  protected:
    virtual void wheelEvent (QWheelEvent *event);

}; // class MapView

} // namespace BajaGIS

#endif // BAJAGIS_MAPVIEW_H
