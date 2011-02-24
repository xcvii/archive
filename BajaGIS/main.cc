#include "MapView.h"

#include <QtGui>

int main (int argc, char *argv[])
{
  using namespace BajaGIS;

  QApplication app (argc, argv);

  QGraphicsScene scene (-1000, -1000, 1000, 1000);
  MapView view (&scene);
  view.setRenderHint (QPainter::Antialiasing);
  view.setViewportUpdateMode (QGraphicsView::BoundingRectViewportUpdate);

  view.resize (400, 300);
  view.show ();

  return app.exec ();
}

