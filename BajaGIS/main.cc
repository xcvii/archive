#include "MapScene.h"
#include "MapView.h"

#include <QtGui>

int main (int argc, char **argv)
{
  using namespace BajaGIS;

  QApplication app (argc, argv);

  MapScene scene (QRectF (0, 0, 1000, 1000));
  scene.setMode (MapScene::InsertMode);
  scene.setShape (Polyline);

  MapView view (&scene);
  view.setRenderHint (QPainter::Antialiasing);
  view.setViewportUpdateMode (QGraphicsView::BoundingRectViewportUpdate);
  view.resize (400, 300);
  view.show ();

  return app.exec ();
}

