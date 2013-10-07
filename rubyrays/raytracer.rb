require_relative 'vector'

class Raytracer
  def initialize(objects = [])
    @objects = objects
  end

  def sample(o, d)
    # find an intersection ray vs world
    m, t, n = trace(o, d)

    if m == :miss_upward
      # the ray hits the sky
      p = 1 - d.z
      p = p * p
      p = p * p
      return Vector.new(0.7, 0.6, 1) * p
    end

    # intersection coordinate
    h = o + (d * t)

    # direction of light
    l = (Vector.new(9 + rand(), 9 + rand(), 16) + (h * -1)).norm

    # b = Lambertian factor
    b = l.dot(n)

    m2, *_ = trace(h, l)

    # illumination factor
    b = 0 if b < 0 || m2 != :miss_upward

    if m == :miss_downward
      # the ray hits the floor
      h = h * 0.2

      if (h.x.ceil + h.y.ceil) & 1 == 1
        pattern = Vector.new(3, 1, 1)
      else
        pattern = Vector.new(3, 3, 3)
      end

      return pattern * (b * 0.2 + 0.1)
    end

    # the half-vector
    r = d + (n * n.dot(d * -2))

    # color with diffuse and specular components
    p = l.dot(r * (b > 0 ? 1 : 0))
    p33 = p * p
    p33 = p33 * p33
    p33 = p33 * p33
    p33 = p33 * p33
    p33 = p33 * p33
    p33 = p33 * p
    p = p33 * p33 * p33

    # a sphere was hit; cast a ray bouncing from the sphere surface
    # and attenuate the color by 50%
    Vector.new(p, p, p) + (sample(h, r) * 0.5)
  end

  private

  def trace(o, d)
    t = 1E9
    m = :miss_upward
    p = -o.z / d.z

    n = Vector::ZEROS

    if (0.01 < p)
      t = p
      m = :miss_downward
      n = Vector::NORMAL
    end

    o = o + Vector.new(0, 3, -4)
    last = nil

    @objects.each do |object|
      p1 = o + object

      b = p1.dot(d)
      c = p1.dot(p1) - 1
      q = b * b - c

      if q > 0
        # the ray intersects the sphere

        # camera-sphere distance
        s = -b - Math.sqrt(q)

        if s < t && s > 0.01
          last = p1
          t = s
          m = :hit
        end
      end
    end

    if last
      # bouncing ray vector
      n = (last + (d * t)).norm
    end

    [m, t, n]
  end
end
