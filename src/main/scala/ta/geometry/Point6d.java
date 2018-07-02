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

public final class Point6d implements HyperPoint {
    public final static int X = 0;
    public final static int Y = 1;
    public final static int Z = 2;
    public final static int A = 3;
    public final static int B = 4;
    public final static int C = 5;

    public final double x, y, z, a, b, c, p;
    public final int o;

    public Point6d(final double x, final double y, final double z, final double a, final double b, final double c, final double p, final int o) {
        this.x = x;
        this.y = y;
        this.z = z;
        this.a = a;
        this.b = b;
        this.c = c;
        this.p = p;
        this.o = o;
    }

    public Point6d(final double x, final double y, final double z, final double a, final double b, final double c) {
        this.x = x;
        this.y = y;
        this.z = z;
        this.a = a;
        this.b = b;
        this.c = c;
        this.p = 0;
        this.o = 0;
    }

    @Override
    public int getNDim() {
        return 6;
    }

    @Override
    public Double getCoord(final int d) {
        if(d==X) {
            return x;
        } else if(d==Y) {
            return y;
        } else if(d==Z) {
            return z;
        } else if(d==A) {
            return a;
        } else if(d==B) {
            return b;
        } else if(d==C) {
            return c;
        } else {
            throw new IllegalArgumentException("Invalid dimension");
        }
    }

    @Override
    public double distance(final HyperPoint p) {
        final Point6d p2 = (Point6d)p;

        final double dx = p2.x-x;
        final double dy = p2.y-y;
        final double dz = p2.z-z;
        final double da = p2.a-a;
        final double db = p2.b-b;
        final double dc = p2.c-c;
        return Math.sqrt(dx*dx + dy*dy + dz*dz + da*da + db*db + dc*dc);
    }

    @Override
    public double distance(final HyperPoint p, final int d) {
        final Point6d p2 = (Point6d)p;
        if(d == X) {
            return Math.abs(p2.x - x);
        } else if (d == Y) {
            return Math.abs(p2.y - y);
        } else if (d == Z) {
            return Math.abs(p2.z - z);
        } else if (d == A) {
            return Math.abs(p2.a - a);
        } else if (d == B) {
            return Math.abs(p2.b - b);
        } else if (d == C) {
            return Math.abs(p2.c - c);
        } else {
            throw new IllegalArgumentException("Invalid dimension");
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        final Point6d p = (Point6d)o;
        return RTree.isEqual(x, p.x) &&
                RTree.isEqual(y, p.y) &&
                RTree.isEqual(z, p.z) &&
                RTree.isEqual(a, p.a) &&
                RTree.isEqual(b, p.b) &&
                RTree.isEqual(c, p.c) &&
                RTree.isEqual(this.p, p.p) &&
                this.o == p.p;
    }

    @Override
    public int hashCode() {
        return Double.hashCode(x) ^
                31*Double.hashCode(y) ^
                31*31*Double.hashCode(z) ^
                31*31*31*Double.hashCode(a) ^
                31*31*31*31*Double.hashCode(b) ^
                31*31*31*31*31*Double.hashCode(c) ^
                31*31*31*Double.hashCode(p) ^
                31*31*Double.hashCode(o);
    }

    public final static class Builder implements RectBuilder<Point6d> {

        @Override
        public HyperRect getBBox(final Point6d point) {
            return new Rect6d(point);
        }

        @Override
        public HyperRect getMbr(final HyperPoint p1, final HyperPoint p2) {
            return new Rect6d((Point6d)p1, (Point6d)p2);
        }
    }
}