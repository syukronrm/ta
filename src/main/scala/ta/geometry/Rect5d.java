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
import collection.spatial.RectBuilder;

import java.util.List;

public final class Rect5d implements HyperRect {
    private Point5d min, max;

    public Rect5d(List<Point5d> points) {
        setPoints(points);
    }

    Rect5d(final Point5d p) {
        min = new Point5d(p.x, p.y, p.z, p.a, p.b);
        max = new Point5d(p.x, p.y, p.z, p.a, p.b);
    }

    public Rect5d(final double x1, final double y1, final double z1, final double a1, final double b1, final double x2, final double y2, final double z2, final double a2, final double b2) {
        min = new Point5d(x1, y1, z1, a1, b1);
        max = new Point5d(x2, y2, z2, a2, b2);
    }

    Rect5d(final Point5d point1, final Point5d point2) {

        final double minX, maxX, minY, maxY, minZ, maxZ, minA, maxA, minB, maxB;

        if(point1.x < point2.x) {
            minX = point1.x;
            maxX = point2.x;
        } else {
            minX = point2.x;
            maxX = point1.x;
        }

        if(point1.y < point2.y) {
            minY = point1.y;
            maxY = point2.y;
        } else {
            minY = point2.y;
            maxY = point1.y;
        }

        if(point1.z < point2.z) {
            minZ = point1.z;
            maxZ = point2.z;
        } else {
            minZ = point2.z;
            maxZ = point1.z;
        }

        if(point1.a < point2.a) {
            minA = point1.a;
            maxA = point2.a;
        } else {
            minA = point2.a;
            maxA = point1.a;
        }

        if(point1.b < point2.b) {
            minB = point1.b;
            maxB = point2.b;
        } else {
            minB = point2.b;
            maxB = point1.b;
        }

        min = new Point5d(minX, minY, minZ, minA, minB);
        max = new Point5d(maxX, maxY, maxZ, maxA, maxB);
    }

    @Override
    public HyperRect getMbr(final HyperRect r) {
        final Rect5d r2 = (Rect5d)r;
        final double minX = Math.min(min.x, r2.min.x);
        final double minY = Math.min(min.y, r2.min.y);
        final double minZ = Math.min(min.z, r2.min.z);
        final double minA = Math.min(min.a, r2.min.a);
        final double minB = Math.min(min.b, r2.min.b);

        final double maxX = Math.max(max.x, r2.max.x);
        final double maxY = Math.max(max.y, r2.max.y);
        final double maxZ = Math.max(max.z, r2.max.z);
        final double maxA = Math.max(max.a, r2.max.a);
        final double maxB = Math.max(max.b, r2.max.b);

        return new Rect5d(minX, minY, minZ, minA, minB, maxX, maxY, maxZ, maxA, maxB);

    }

    @Override
    public int getNDim() {
        return 5;
    }

    @Override
    public HyperPoint getCentroid() {
        final double dx = min.x + (max.x - min.x)/2.0;
        final double dy = min.y + (max.y - min.y)/2.0;
        final double dz = min.z + (max.z - min.z)/2.0;
        final double da = min.a + (max.a - min.a)/2.0;
        final double db = min.b + (max.b - min.b)/2.0;

        return new Point5d(dx, dy, dz, da, db);
    }

    @Override
    public HyperPoint getMin() {
        return min;
    }

    @Override
    public HyperPoint getMax() {
        return max;
    }

    @Override
    public double getRange(final int d) {
        if(d == 0) {
            return max.x - min.x;
        } else if(d == 1) {
            return max.y - min.y;
        } else if(d == 2) {
            return max.z - min.z;
        } else if(d == 3) {
            return max.a - min.a;
        } else if(d == 4) {
            return max.b - min.b;
        } else {
            throw new IllegalArgumentException("Invalid dimension");
        }
    }

    @Override
    public boolean contains(final HyperRect r) {
        final Rect5d r2 = (Rect5d)r;

        return min.x <= r2.min.x &&
                max.x >= r2.max.x &&
                min.y <= r2.min.y &&
                max.y >= r2.max.y &&
                min.z <= r2.min.z &&
                max.z >= r2.max.z &&
                min.a <= r2.min.a &&
                max.a >= r2.max.a &&
                min.b <= r2.min.b &&
                max.b >= r2.max.b;
    }

    @Override
    public boolean intersects(final HyperRect r) {
        final Rect5d r2 = (Rect5d)r;

        return !(min.x > r2.max.x) &&
                !(r2.min.x > max.x) &&
                !(min.y > r2.max.y) &&
                !(r2.min.y > max.y) &&
                !(min.z > r2.max.z) &&
                !(r2.min.z > max.z) &&
                !(min.a > r2.max.a) &&
                !(r2.min.a > max.a) &&
                !(min.b > r2.max.b) &&
                !(r2.min.b > max.b);
    }

    @Override
    public double cost() {
        final double dx = max.x - min.x;
        final double dy = max.y - min.y;
        final double dz = max.z - min.z;
        final double da = max.a - min.a;
        final double db = max.b - min.b;
        return Math.abs(dx*dy*dz*da*db);
    }

    @Override
    public double perimeter() {
        double p = 0.0;
        final int nD = this.getNDim();
        for(int d = 0; d<nD; d++) {
            p += 4.0 * this.getRange(d);
        }
        return p;
    }

    @Override
    public HyperRect getPDR() {
        final double minX = min.x;
        final double minY = min.y;
        final double minZ = min.z;
        final double minA = min.a;
        final double minB = min.b;
        final double maxX = Double.MAX_VALUE;
        final double maxY = Double.MAX_VALUE;
        final double maxZ = Double.MAX_VALUE;
        final double maxA = Double.MAX_VALUE;
        final double maxB = Double.MAX_VALUE;

        return new Rect5d(minX, minY, minZ, minA, minB, maxX, maxY, maxZ, maxA, maxB);
    }

    @Override
    public HyperRect getDDR() {
        final double minX = max.x;
        final double minY = max.y;
        final double minZ = max.z;
        final double minA = max.a;
        final double minB = max.b;
        final double maxX = Double.MAX_VALUE;
        final double maxY = Double.MAX_VALUE;
        final double maxZ = Double.MAX_VALUE;
        final double maxA = Double.MAX_VALUE;
        final double maxB = Double.MAX_VALUE;

        return new Rect5d(minX, minY, minZ, minA, minB, maxX, maxY, maxZ, maxA, maxB);
    }

    @Override
    public HyperRect getPDD() {
        final double minX = Double.MIN_VALUE;
        final double minY = Double.MIN_VALUE;
        final double minZ = Double.MIN_VALUE;
        final double minA = Double.MIN_VALUE;
        final double minB = Double.MIN_VALUE;
        final double maxX = max.x;
        final double maxY = max.y;
        final double maxZ = max.z;
        final double maxA = max.a;
        final double maxB = max.a;

        return new Rect5d(minX, minY, minZ, minA, minB, maxX, maxY, maxZ, maxA, maxB);
    }

    @Override
    public HyperRect getDDD() {
        final double minX = Double.MIN_VALUE;
        final double minY = Double.MIN_VALUE;
        final double minZ = Double.MIN_VALUE;
        final double minA = Double.MIN_VALUE;
        final double minB = Double.MIN_VALUE;
        final double maxX = min.x;
        final double maxY = min.y;
        final double maxZ = min.z;
        final double maxA = min.a;
        final double maxB = min.b;

        return new Rect5d(minX, minY, minZ, minA, minB, maxX, maxY, maxZ, maxA, maxB);
    }

    @Override
    public void setPoints(List points) {
        double minX, minY, minZ, minA, minB, maxX, maxY, maxZ, maxA, maxB;
        minX = Double.MAX_VALUE;
        minY = Double.MAX_VALUE;
        minZ = Double.MAX_VALUE;
        minA = Double.MAX_VALUE;
        minB = Double.MAX_VALUE;
        maxX = Double.MIN_VALUE;
        maxY = Double.MIN_VALUE;
        maxZ = Double.MIN_VALUE;
        maxA = Double.MIN_VALUE;
        maxB = Double.MIN_VALUE;

        for (Object point : points) {
            Point5d p = (Point5d) point;

            if (p.x < minX) {
                minX = p.x;
            } else if (p.x > maxX) {
                maxX = p.x;
            }

            if (p.y < minY) {
                minY = p.y;
            } else if (p.y > maxY) {
                maxY = p.y;
            }

            if (p.z < minZ) {
                minZ = p.z;
            } else if (p.z > maxZ) {
                maxZ = p.z;
            }

            if (p.a < minA) {
                minA = p.a;
            } else if (p.a > maxA) {
                maxA = p.a;
            }

            if (p.b < minB) {
                minB = p.b;
            } else if (p.b > maxB) {
                maxB = p.b;
            }
        }

        min = new Point5d(minX, minY, minZ, minA, minB);
        max = new Point5d(maxX, maxY, maxZ, maxA, maxB);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        final Rect5d rect3d = (Rect5d) o;

        return min.equals(rect3d.min) &&
                max.equals(rect3d.max);
    }

    @Override
    public int hashCode() {
        return min.hashCode() ^ 31*max.hashCode();
    }

    public String toString() {

        return "(" +
                Double.toString(min.x) +
                ',' +
                Double.toString(min.y) +
                ',' +
                Double.toString(min.z) +
                ')' +
                ' ' +
                '(' +
                Double.toString(max.x) +
                ',' +
                Double.toString(max.y) +
                ',' +
                Double.toString(max.z) +
                ')';
    }

    public final static class Builder implements RectBuilder<Rect5d> {

        @Override
        public HyperRect getBBox(final Rect5d rect) {
            return rect;
        }

        @Override
        public HyperRect getMbr(final HyperPoint p1, final HyperPoint p2) {
            return new Rect5d(p1.getCoord(0), p1.getCoord(1), p1.getCoord(2), p1.getCoord(3), p1.getCoord(4), p2.getCoord(0), p2.getCoord(1), p2.getCoord(2), p2.getCoord(3), p2.getCoord(4));
        }
    }
}