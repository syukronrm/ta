package ta.geometry;

/*
 * #%L
 * Conversant RTree
 * ~~
 * Conversantmedia.com © 2016, Conversant, Inc. Conversant® is a trademark of Conversant, Inc.
 * ~~
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 *
 * Modifications copyright (C) 2018 <syukronrm>
 */

import collection.spatial.HyperPoint;
import collection.spatial.HyperRect;
import collection.spatial.RTree;
import collection.spatial.RectBuilder;

/**
 * Created by jcovert on 6/15/15.
 */
public final class Point2d implements HyperPoint {
    public static final int X = 0;
    public static final int Y = 1;

    public final double x, y, p;
    public final int o;

    public Point2d(final double x, final double y, final double p, final int o) {
        this.x = x;
        this.y = y;
        this.p = p;
        this.o = o;
    }

    public Point2d(final double x, final double y) {
        this.x = x;
        this.y = y;
        this.p = 0;
        this.o = 0;
    }

    @Override
    public int getNDim() {
        return 2;
    }

    @Override
    public Double getCoord(final int d) {
        if(d==X) {
            return x;
        } else if(d==Y) {
            return y;
        } else {
            throw new IllegalArgumentException("Invalid dimension");
        }
    }

    @Override
    public double distance(final HyperPoint p) {
        final Point2d p2 = (Point2d)p;

        final double dx = p2.x-x;
        final double dy = p2.y-y;
        return Math.sqrt(dx*dx + dy*dy);
    }

    @Override
    public double distance(final HyperPoint p, final int d) {
        final Point2d p2 = (Point2d)p;
        if(d == X) {
            return Math.abs(p2.x - x);
        } else if (d == Y) {
            return Math.abs(p2.y - y);
        } else {
            throw new IllegalArgumentException("Invalid dimension");
        }
    }

    public boolean equals(final Object o) {
        if(this == o) return true;
        if(o==null || getClass() != o.getClass()) return false;

        final Point2d p = (Point2d) o;
        return RTree.isEqual(x, p.x) &&
                RTree.isEqual(y, p.y) &&
                RTree.isEqual(this.p, p.p) &&
                this.o == p.p;
    }


    public int hashCode() {
        return Double.hashCode(x) ^ 31*Double.hashCode(y) ^ 31*31*Double.hashCode(p) ^ 31*31*Double.hashCode(o);
    }

    public final static class Builder implements RectBuilder<Point2d> {

        @Override
        public HyperRect getBBox(final Point2d point) {
            return new Rect2d(point);
        }

        @Override
        public HyperRect getMbr(final HyperPoint p1, final HyperPoint p2) {
            final Point2d point1 = (Point2d)p1;
            final Point2d point2 = (Point2d)p2;
            return new Rect2d(point1, point2);
        }
    }
}