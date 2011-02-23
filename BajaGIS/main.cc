#include "BGViewport.h"

#include <QtGui>

int main (int argc, char *argv[])
{
  using namespace BajaGIS;

  QApplication app (argc, argv);

  QGraphicsScene scene (-1000, -1000, 1000, 1000);
  BGViewport view (&scene);
  view.setRenderHint (QPainter::Antialiasing);
  view.setViewportUpdateMode (QGraphicsView::BoundingRectViewportUpdate);

  view.resize (640, 480);
  view.show ();

  return app.exec ();
}

