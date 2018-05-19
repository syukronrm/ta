import collection.spatial.HyperRect;
import collection.spatial.HyperPoint;
import collection.spatial.RectBuilder;

public class PointQ implements HyperPoint {
    final double x, y;

    public PointQ(final double x, final double y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public int getNDim() { return 2; }

    @Override
    public Double getCoord(final int d) {
        if(d==0) { return x; }
        else if(d==1) { return y; }
        else { throw new RuntimeException("Invalid dimension"); }
    }

    @Override
    public double distance(final HyperPoint p) {
        final PointQ p2 = (PointQ)p;
        final double dx = p2.x - x;
        final double dy = p2.y - y;
        return Math.sqrt(dx*dx + dy*dy);
    }

    @Override
    public double distance(final HyperPoint p, final int d) {
        final PointQ p2 = (PointQ)p;
        if (d == 0) { return Math.abs(p2.x - x); }
        else if (d == 1) { return Math.abs(p2.y - y); }
        else { throw new RuntimeException("Invalid dimension"); }
    }

    public final static class Builder implements RectBuilder<PointQ> {
        @Override
        public HyperRect getBBox(final PointQ point) { return new Rect2D(point); }

        @Override
        public HyperRect getMbr(final HyperPoint p1, final HyperPoint p2) {
            final PointQ point1 = (PointQ)p1;
            final PointQ point2 = (PointQ)p2;
            return new Rect2D(point1, point2);
        }
    }
}