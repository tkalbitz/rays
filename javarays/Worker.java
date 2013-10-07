package javarays;

import static javarays.Raycaster.aspectRatio;
import static javarays.Raycaster.bytes;

import java.util.concurrent.ThreadLocalRandom;

import javarays.Raycaster.vector;

final class Worker implements Runnable  {

    // Default pixel color is almost pitch black
    private final vector DEFAULT_COLOR    = new vector(13, 13, 13);

    private static final vector EMPTY_VEC = new vector();
    private static final vector SKY_VEC   = new vector(1.f,  1.f,  1.f);

    // Ray Origin
    private static final vector CAM_FOCAL_VEC   = new vector(16.f, 16.f,  8.f);
    private static final vector T_CONST_VEC     = new vector( 0.f,  3.f, -4.f);
    private static final vector FLOOR_PATTERN_1 = new vector( 3.f,  1.f,  1.f);
    private static final vector FLOOR_PATTERN_2 = new vector( 3.f,  3.f,  3.f);

    private static final vector STD_VEC   = new vector(0.f,  0.f,  1.f);
    private static final vector g = (new vector(-3.1f, -16.f, 3.2f)).norm(); // WTF ? See https://news.ycombinator.com/item?id=6425965 for more.

    private static final vector a = (STD_VEC.cross(g)).norm().scale(.002f);
    private static final vector b = (g.cross(a)).norm().scale(.002f);
    private static final vector c = (a.add(b)).scale(-256).add(g);

    private final int offset;
    private final int jump;

    // for stochastic sampling
    private final ThreadLocalRandom rnd = ThreadLocalRandom.current();

    private final vector[] objects;

    private float t;
    private vector n;

    public Worker(final vector[] _objects, final int _offset, final int _jump) {
        objects = _objects;
        offset = _offset;
        jump = _jump;
    }

    //The intersection test for line [orig, v].
    // Return 2 if a hit was found (and also return distance t and bouncing ray n).
    // Return 0 if no hit was found but ray goes upward
    // Return 1 if no hit was found but ray goes downward
    private final int tracer(vector orig, final vector dir) {
        t = 1e9f;
        int m = 0;
        final float p = -orig.z / dir.z;

        n = EMPTY_VEC;
        if (.01f < p) {
            t = p;
            n = STD_VEC;
            m = 1;
        }

        orig = orig.add(T_CONST_VEC);
        vector last = null;
        for(int i = 0; i < objects.length; i++) {
            // There is a sphere but does the ray hits it ?
            final vector p1 = orig.add(objects[i]);
            final float b = p1.dot(dir);
            final float c = p1.dot(p1) - 1;
            final float b2 = b * b;

            // Does the ray hit the sphere ?
            if (b2 > c) {
                // It does, compute the distance camera-sphere
                final float q = b2 - c;
                final float s = (float) (-b - Math.sqrt(q));

                if (s < t && s > .01f) {
                    last = p1;
                    t = s;
                    m = 2;
                }
            }
        }

        if(last != null) {
            n = (last.add(dir.scale(t))).norm();
        }

        return m;
    }

    // Sample the world and return the pixel color for
    // a ray passing by point origin and dir (Direction)
    private final vector sample(final vector origin, final vector dir) {
        // Search for an intersection ray Vs World.
        final int m = tracer(origin, dir);
        final vector on = new vector(n);

        if (m == 0) { // m==0
            // No sphere found and the ray goes upward: Generate a sky color
            final float p = 1 - dir.z;
            return SKY_VEC.scale(p);
        }

        // A sphere was maybe hit.
        vector h = origin.add(dir.scale(t)); // h = intersection coordinate

        // 'l' = direction to light (with random delta for soft-shadows).
        final vector l = new vector(9.f + rnd.nextFloat(),
                                    9.f + rnd.nextFloat(),
                                    16.f).add(h.scale(-1.f)).norm();

        // Calculated the lambertian factor
        float b = l.dot(n);

        // Calculate illumination factor (lambertian coefficient > 0 or in shadow)?
        if (b < 0 || tracer(h, l) != 0) {
            b = 0;
        }

        if (m == 1) { // m == 1
            h = h.scale(.2f); // No sphere was hit and the ray was going downward: Generate a floor color
            final boolean cond = ((int) (Math.ceil(h.x) + Math.ceil(h.y)) & 1) == 1;
            return (cond ? FLOOR_PATTERN_1 : FLOOR_PATTERN_2).scale(b * .2f + .1f);
        }

        final vector r = dir.add(on.scale(on.dot(dir.scale(-2.f)))); // r = The half-vector

        // Calculate the color 'p' with diffuse and specular component
        float p = l.dot(r.scale(b > 0 ? 1.f : 0.f));
        float p33 = p * p;
        p33 = p33 * p33;
        p33 = p33 * p33;
        p33 = p33 * p33;
        p33 = p33 * p33;
        p33 = p33 * p;
        p = p33 * p33 * p33;

        // m == 2 A sphere was hit. Cast an ray bouncing from the sphere surface.
        // Attenuate color by 50% since it is bouncing (*.5)
        return new vector(p, p, p).add(sample(h, r).scale(.5f));
    }

    @Override
    public void run() {
        for (int y = offset; y < Raycaster.size; y += jump) { // For each row
            int k = (Raycaster.size - y - 1) * Raycaster.size * 3;

            for (int x = Raycaster.size; x-- > 0 ; ) { // For each pixel in a line
                // Reuse the vector class to store not XYZ but a RGB pixel color
                final vector p = innerLoop(y, x, DEFAULT_COLOR);
                bytes[k++] = (byte) p.x;
                bytes[k++] = (byte) p.y;
                bytes[k++] = (byte) p.z;
            }
        }
    }

    private vector innerLoop(final int y, final int x, vector p) {
        // Cast 64 rays per pixel (For blur (stochastic sampling)
        // and soft-shadows.
        for (int r = 0; r < 64; r++) {
            // The delta to apply to the origin of the view (For
            // Depth of View blur).
            final float factor1 = (rnd.nextFloat()-.5f) * 99.f;
            final float factor2 = (rnd.nextFloat()-.5f) * 99.f;
            final vector t = a.scale(factor1).add(b.scale(factor2)); // A little bit of delta up/down and left/right

            // Set the camera focal point vector(17,16,8) and Cast the ray
            // Accumulate the color returned in the p variable

            // Ray Direction with random deltas
            final vector tmpA = a.scale(rnd.nextFloat() + x * aspectRatio);
            final vector tmpB = b.scale(rnd.nextFloat() + y * aspectRatio);
            final vector tmpC = tmpA.add(tmpB).add(c);
            final vector rayDirection = t.scale(-1).add(tmpC.scale(16.f)).norm();

            p = sample(CAM_FOCAL_VEC.add(t), rayDirection).scale(3.5f).add(p); // +p for color accumulation
        }
        return p;
    }
}
