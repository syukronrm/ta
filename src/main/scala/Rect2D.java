import collection.spatial.HyperRect;
import collection.spatial.HyperPoint;
import collection.spatial.RectBuilder;
import collection.spatial.RTree;

/**
 * Created by jcovert on 6/15/15.
 */
public class Rect2D implements HyperRect {
    private final PointQ min, max;

    public Rect2D(final PointQ p) {
        min = new PointQ(p.x, p.y);
        max = new PointQ(p.x, p.y);
    }

    public Rect2D(final double x1, final double y1, final double x2, final double y2) {
        min = new PointQ(x1, y1);
        max = new PointQ(x2, y2);
    }

    public Rect2D(final PointQ p1, final PointQ p2) {
        final double minX, minY, maxX, maxY;
        if(p1.x < p2.x) {
            minX = p1.x;
            maxX = p2.x;
        } else {
            minX = p2.x;
            maxX = p2.x;
        }
        if(p1.y < p2.y) {
            minY = p1.y;
            maxY = p2.y;
        } else {
            minY = p2.y;
            maxY = p2.y;
        }
        min = new PointQ(minX, minY);
        max = new PointQ(maxX, maxY);
    }

    @Override
    public HyperRect getMbr(final HyperRect r) {
        final Rect2D r2 = (Rect2D)r;
        final double minX = Math.min(min.x, r2.min.x);
        final double minY = Math.min(min.y, r2.min.y);
        final double maxX = Math.max(max.x, r2.max.x);
        final double maxY = Math.max(max.y, r2.max.y);
        return new Rect2D(minX, minY, maxX, maxY);
    }

    @Override
    public int getNDim() { return 2; }

    @Override
    public HyperPoint getCentroid() {
        final double dx = min.x + (max.x - min.x)/2.0;
        final double dy = min.y + (max.y - min.y)/2.0;
        return new PointQ(dx, dy);
    }

    @Override
    public HyperPoint getMin() { return min; }

    @Override
    public HyperPoint getMax() { return max; }

    @Override
    public double getRange(final int d) {
        if(d == 0) { return max.x - min.x; }
        else if(d == 1) { return max.y - min.y; }
        else { throw new RuntimeException(("Invalid dimension")); }
    }

    @Override
    public boolean contains(final HyperRect r) {
        final Rect2D r2 = (Rect2D)r;
        return min.x <= r2.min.x &&
                max.x >= r2.max.x &&
                min.y <= r2.min.y &&
                max.y >= r2.max.y;
    }

    @Override
    public boolean intersects(final HyperRect r) {
        final Rect2D r2 = (Rect2D)r;
        if (min.x > r2.max.x ||
                r2.min.x > max.x ||
                min.y > r2.max.y ||
                r2.min.y > max.y) {
            return false;
        }
        return true;
    }

    @Override
    public double cost() {
        final double dx = max.x - min.x;
        final double dy = max.y - min.y;
        return Math.abs(dx) * Math.abs(dy);
    }

    public final static class Builder implements RectBuilder<Rect2D> {
        @Override
        public HyperRect getBBox(final Rect2D rect2D) { return rect2D; }

        @Override
        public HyperRect getMbr(final HyperPoint p1, final HyperPoint p2) {
            return new Rect2D(p1.getCoord(0), p1.getCoord(1), p2.getCoord(0), p2.getCoord(1));
        }
    }

    @Override
    public double perimeter(){
        final double dx = max.x - min.x;
        final double dy = max.y - min.y;

        return Math.sqrt(dx * dx + dy + dy);
    }
}