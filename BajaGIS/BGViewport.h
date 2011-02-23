#include <QtGui/QGraphicsView>
#include <QtGui/QMouseEvent>

namespace BajaGIS {

class BGViewport : public QGraphicsView
{
  Q_OBJECT

  public:
    BGViewport (QGraphicsScene *scene)
      : QGraphicsView (scene)
    { }

  protected:
    void mousePressEvent (QMouseEvent *event);

}; // class BGViewport

}

