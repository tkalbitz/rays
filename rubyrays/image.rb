require_relative 'vector'

class Image
  DEFAULT_COLOR = Vector.new(13, 13, 13)

  attr_reader :data

  def initialize(width, height)
    @width, @height = width, height
    @data = Array.new(3 * width * height, 0)
  end

  def []=(i, value)
    @data[i] = [value, 255].min
  end

  def save(pathname = 'render.ppm')
    File.write(pathname, to_ppm)
  end

  def to_ppm
    # binary portable pixmap
    header = "P6\n%d %d\n255\n" % [@width, @height]
    header + @data.pack('C*')
  end
end
