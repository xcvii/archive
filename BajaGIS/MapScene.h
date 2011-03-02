#ifndef BAJAGIS_MAPSCENE_H
#define BAJAGIS_MAPSCENE_H

#include "Shape.h"

#include <QtGui>

namespace BajaGIS {

class MapScene : public QGraphicsScene
{
  Q_OBJECT

  public:
    enum Mode { InsertMode, MoveMode };

    MapScene (QRectF const &rect, QObject *parent = 0);

  public slots:
    void setMode (Mode mode) { _mode = mode; }
    void setShape (ShapeType shape) { _shapeMode = shape; }

  protected:
    void mousePressEvent (QGraphicsSceneMouseEvent *event);
    void mouseMoveEvent (QGraphicsSceneMouseEvent *event);

  private:
    Mode _mode;
    ShapeType _shapeMode;
    Shape *_currentShape;
    //QGraphicsLineItem _dummyLine;

    void _insert (qreal x, qreal y);

}; // class MapScene

} // namespace BajaGIS

#endif // BAJAGIS_MAPSCENE_H
