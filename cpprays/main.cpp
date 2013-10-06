#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <cstring>
#include <random>
#include <thread>
#include <vector>
#include <string>

//Define a vector class with constructor and operator: 'v'
struct vector {
  float x,y,z;  // Vector has three float attributes.
  vector operator+(vector r) const {return vector(x+r.x,y+r.y,z+r.z);} //Vector add
  vector operator*(float r) const {return vector(x*r,y*r,z*r);}       //Vector scaling
  float operator%(vector r) const {return x*r.x+y*r.y+z*r.z;}    //Vector dot product
  vector(){}                                  //Empty constructor
  vector operator^(vector r) const {return vector(y*r.z-z*r.y,z*r.x-x*r.z,x*r.y-y*r.x);} //Cross-product
  vector(float a,float b,float c){x=a;y=b;z=c;}            //Constructor
  vector operator!() const {return *this*(1/sqrtf(*this%*this));} // Used later for normalizing the vector
};

struct object {
  float k,j;
  object(float x,float y){k=x;j=y;}
};

using Objects = std::vector<object>;

Objects objects;

using Art = std::vector<std::string>;

Objects makeObjects(const Art& art) {
  Objects o;
  auto y = 1.0f - static_cast<float>(art.size());
  for(const auto& line : art) {
    auto x = 1.0f - static_cast<float>(line.size());
    for(const auto& c : line) {
      if(' ' != c) {
        o.emplace_back(x, y);
      }
      x += 1.0f;
    }
    y += 1.0f;
  }
  return o;
}

float R(unsigned int& seed) {
  seed += seed;
  seed ^= 1;
  if ((int)seed < 0)
    seed ^= 0x88888eef;
  return (float)(seed % 95) / (float)95;
}

int T(vector o,vector d,float& t,vector& n) {
  t=1e9;
  int m=0;
  const float p=-o.z/d.z;

  if(.01f<p)
    t=p,n=vector(0,0,1),m=1;

  o=o+vector(0,3,-4);
  for (const auto& obj : objects) {
    const vector p=o+vector(obj.k,0,obj.j);
    const float b=p%d,c=p%p-1,b2=b*b;

    if(b2>c) {
      const float q=b2-c, s=-b-sqrtf(q);

      if(s<t && s>.01f)
        t=s, n=!(p+d*t), m=2;
    }
  }

  return m;
}

vector S(vector o,vector d, unsigned int& seed) {
  float t;
  vector n;

  //Search for an intersection ray Vs World.
  const int m=T(o,d,t,n);
  const vector on = n;

  if(!m) { // m==0
    float p = 1-d.z;
    p = p*p;
    p = p*p;
    return vector(.7f,.6f,1)*p;
  }

  vector h=o+d*t,
    l=!(vector(9+R(seed),9+R(seed),16)+h*-1);

  float b=l%n;

  if(b<0||T(h,l,t,n))
    b=0;

  if(m&1) {   //m == 1
    h=h*.2f;
    return((int)(ceil(h.x)+ceil(h.y))&1?vector(3,1,1):vector(3,3,3))*(b*.2f+.1f);
  }

  const vector r=d+on*(on%d*-2);               // r = The half-vector

  float p=l%r*(b>0);
  float p33 = p*p;
  p33 = p33*p33;
  p33 = p33*p33;
  p33 = p33*p33;
  p33 = p33*p33;
  p33 = p33*p;
  p = p33*p33*p33;

  return vector(p,p,p)+S(h,r,seed)*.5;
}

int main(int argc, char **argv) {
  const Art art {
    "                   ",
    "    1111           ",
    "   1    1          ",
    "  1           11   ",
    "  1          1  1  ",
    "  1     11  1    1 ",
    "  1      1  1    1 ",
    "   1     1   1  1  ",
    "    11111     11   "
  };

  objects = makeObjects(art);

  const auto getIntArg = [&](int argIndex, int defaultValue) {
    if(argc > argIndex) {
      return std::stoi(argv[argIndex]);
    }
    return defaultValue;
  };

  const auto w = getIntArg(1, 512);
  const auto h = getIntArg(2, 512);
  const auto num_threads = [&]() {
    int x = getIntArg(3, 0);
    if(x <= 0) {
      x = std::thread::hardware_concurrency();
      if(0 == x) {
        //8 threads is a reasonable assumption if we don't know how many cores there are
        x = 8;
      }
    }
    return x;
  }();

  printf("P6 %d %d 255 ", w, h); // The PPM Header is issued

  const vector g=!vector(-5.5f,-16,0),
    a=!(vector(0,0,1)^g) * .002f,
    b=!(g^a)*.002f,
    c=(a+b)*-256+g;

  std::vector<char> bytes(3 * w * h);

  auto lambda=[&](unsigned int seed, int offset, int jump) {
    for (int y=offset; y<h; y+=jump) {    //For each row
      int k = (h - y - 1) * w * 3;

      for(int x=w;x--;) {
        vector p(13,13,13);

        for(int r=64;r--;) {
          const vector t=a*(R(seed)-.5f)*99+b*(R(seed)-.5f)*99;

          p=S(vector(17,16,8)+t,
            !(t*-1+(a*(R(seed)+x)+b*(y+R(seed))+c)*16),
            seed)*3.5f+p;
        }

        bytes[k++] = (char)p.x;
        bytes[k++] = (char)p.y;
        bytes[k++] = (char)p.z;
      }
    }
  };

  std::mt19937 rgen;
  std::vector<std::thread> threads;
  for(int i=0;i<num_threads;++i) {
    threads.emplace_back(lambda, rgen(), i, num_threads);
  }
  for(auto& t : threads) {
    t.join();
  }

  fwrite(bytes.data(), sizeof(bytes[0]), bytes.size(), stdout);
}
