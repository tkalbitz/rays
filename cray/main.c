#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <pthread.h>

typedef struct {
  float x,y,z;  // Vector has three float attributes.
} vector;

inline vector cvector(const float x, const float y, const float z) { vector t = { .x = x, .y = y, .z = z}; return t; }

#define ADD(a, b) cvector((a).x+(b).x, (a).y+(b).y, (a).z+(b).z)
#define DOT(a, b) ((a).x*(b).x+(a).y*(b).y+(a).z*(b).z)
#define MUL(a, b) cvector((a).x*(b), (a).y*(b), (a).z*(b))

inline vector add  (const vector a, const vector b) { vector t = { .x = a.x+b.x, .y = a.y+b.y, .z = a.z+b.z}; return t; }
inline vector mul  (const vector a, const float  b) { vector t = { .x = a.x*b, .y = a.y*b, .z = a.z*b}; return t; }
inline vector cross(const vector a, const vector b) { vector t = { .x = a.y*b.z-a.z*b.y, .y = a.z*b.x-a.x*b.z, .z = a.x*b.y-a.y*b.x}; return t; }
inline float  dot  (const vector a, const vector b) { return a.x*b.x+a.y*b.y+a.z*b.z; }
inline vector norm (const vector a)           { return mul(a, (1/sqrtf(DOT(a, a)))); }

int w = 512, h = 512;
vector g, a, b, c;

const char *art[] = {
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

#define ORBS_COUNT 32
vector objects[ORBS_COUNT];
const int object_count = ORBS_COUNT;

void F() {
  const int nr = sizeof(art) / sizeof(char *);
  const int nc = strlen(art[0]);

  int counter = 0;

  for (int k = nc - 1; k >= 0; k--) {
    for (int j = nr - 1; j >= 0; j--) {
      if(art[j][nc - 1 - k] != ' ') {
        objects[counter++] = cvector(-k, 0.f, -(nr - 1 - j));
      }
    }
  }
}

float R(unsigned int* _seed) {
  unsigned int seed = *_seed;

  seed += seed;
  seed ^= 1;
  if ((int)seed < 0)
    seed ^= 0x88888eef;
  *_seed = seed;
  return (float)(seed % 95) / (float)95;
}

//The intersection test for line [o,v].
// Return 2 if a hit was found (and also return distance t and bouncing ray n).
// Return 0 if no hit was found but ray goes upward
// Return 1 if no hit was found but ray goes downward
int T(vector o, const vector* d, float* t, vector* n) {
  const static vector vec1 = {0.f, 0.f,  1.f};
  const static vector vec2 = {0.f, 3.f, -4.f};

  int m = 0;
  float p = -o.z / d->z;
  *t = 1e9;

  if(.01f < p)
    *t = p, *n = vec1, m = 1;

  o = ADD(o, vec2);
  for (int i = 0; i < object_count; i++) {
    // There is a sphere but does the ray hits it ?
    vector p = ADD(o, objects[i]);
    float b  = DOT(p, *d), c = DOT(p, p) - 1, b2 = b * b;

    // Does the ray hit the sphere ?
    if(b2 > c) {
      //It does, compute the distance camera-sphere
      float q = b2 - c, s = -b - sqrtf(q);

      if(s < *t && s > .01f)
      // So far this is the minimum distance, save it. And also
      // compute the bouncing ray vector into 'n'
      *t = s, *n = norm(add(p, MUL(*d, *t))), m = 2;
    }
  }

  return m;
}

// (S)ample the world and return the pixel color for
// a ray passing by point o (Origin) and d (Direction)
vector S(const vector* o, const vector* d, unsigned int* seed) {
  float t;
  vector n, on;

  //Search for an intersection ray Vs World.
  int m = T(*o, d, &t, &n);
  on = n;

  if(!m) { // m==0
    //No sphere found and the ray goes upward: Generate a sky color
    float p = 1 - d->z;
    p = p * p;
    p = p * p;
    return mul(cvector(.7f, .6f, 1), p);
  }

  //A sphere was maybe hit.
  vector h = add(*o, mul(*d, t));  // h = intersection coordinate
  vector l = norm( add( cvector(9 + R(seed), 9 + R(seed), 16), mul(h, -1)));  // 'l' = direction to light (with random delta for soft-shadows).

  //Calculated the lambertian factor
  float b = DOT(l, n);

  //Calculate illumination factor (lambertian coefficient > 0 or in shadow)?
  if(b < 0 || T(h, &l, &t, &n))
    b = 0;

  if(m & 1) {   //m == 1
    h = mul(h, .2f); //No sphere was hit and the ray was going downward: Generate a floor color
    return mul( ((int)(ceil(h.x) + ceil(h.y)) & 1 ? cvector(3,1,1) : cvector(3,3,3)), (b * .2f + .1f));
  }

  vector r = add(*d, mul(on, dot(on, mul(*d, -2)))); // r = The half-vector

  // Calculate the color 'p' with diffuse and specular component
  float p = dot(l, mul(r, (b > 0)));
  float p33 = p * p;
  p33 = p33 * p33;
  p33 = p33 * p33;
  p33 = p33 * p33;
  p33 = p33 * p33;
  p33 = p33 * p;
  p = p33 * p33 * p33;

  //m == 2 A sphere was hit. Cast an ray bouncing from the sphere surface.
  return add(cvector(p, p, p), mul(S(&h, &r, seed), .5f)); //Attenuate color by 50% since it is bouncing (* .5)
}

typedef struct {
  unsigned int seed;
  int offset;
  int jump;
  char* bytes;
} params;

void* worker_thread(void* args) {
  params* param = (params*)args;
  int offset        = param->offset;
  int jump          = param->jump;
  char* bytes       = param->bytes;
  unsigned int seed = param->seed;
  free(param);

  for (int y = offset; y < h; y += jump) {    //For each row
    int k = (h - y - 1) * w * 3;

    for(int x = w; x--;) {   //For each pixel in a line
      //Reuse the vector class to store not XYZ but a RGB pixel color
      vector p = {.x = 13.f, .y = 13.f, .z = 13.f}; // Default pixel color is almost pitch black

      //Cast 64 rays per pixel (For blur (stochastic sampling) and soft-shadows.
      for(int r = 64; r--;) {
        // The delta to apply to the origin of the view (For Depth of View blur).
        vector t = add(mul(mul(a, (R(&seed) - .5f)), 99.f),
                       mul(mul(b, (R(&seed) - .5f)), 99.f)); // A little bit of delta up/down and left/right

        // Set the camera focal point vector(17,16,8) and Cast the ray
        // Accumulate the color returned in the p variable
	const vector tmp1 = add(cvector(17.f, 16.f, 8.f), t); // ray origin
	const vector tmp2 = norm(add(
			mul(t, -1.f),
			mul(
			    add(	
				add(
				    mul(a, (R(&seed) + x)), 
				    mul(b, (R(&seed) + y))),
				c)
			, 16))); // Ray Direction with random deltas
        p = add(mul(S(&tmp1, &tmp2, &seed)  // for stochastic sampling
               , 3.5f), p); // +p for color accumulation
      }

      bytes[k++] = (char)p.x;
      bytes[k++] = (char)p.y;
      bytes[k++] = (char)p.z;
    }
  }

  pthread_exit(NULL);
}

// The main function. It generates a PPM image to stdout.
// Usage of the program is hence: ./card > erk.ppm
int main(int argc, char **argv) {
  F();
  srand(time(NULL));

  int num_threads = sysconf(_SC_NPROCESSORS_ONLN);
  if (num_threads <= 0) {
    fprintf(stderr, "Can't get number of cores. Fallback to 8.\n");
    //8 threads is a reasonable assumption if we don't know how many cores there are
    num_threads = 8;
  }

  if (argc > 1) 
    w = atoi(argv[1]);

  if (argc > 2) 
    h = atoi(argv[2]);

  if (argc > 3) 
    num_threads = atoi(argv[3]);

  g = norm(cvector(-5.5f, -16, 0)),           // Camera direction
  a = mul(norm(cross(cvector(0, 0, 1), g)), .002f), // Camera up vector...Seem Z is pointing up :/ WTF !
  b = mul(norm(cross(g, a)), .002f),               // The right vector, obtained via traditional cross-product
  c = add(mul(add(a, b), -256), g);                // WTF ? See https://news.ycombinator.com/item?id=6425965 for more.

  printf("P6 %d %d 255 ", w, h); // The PPM Header is issued

  int s = 3 * w * h;
  char* bytes = malloc(s * sizeof(char));
  pthread_t* threads = malloc(num_threads * sizeof(pthread_t));

  for(int i = 0; i < num_threads; ++i) {
    params* p = malloc(sizeof(*p));
    p->offset = i;
    p->jump   = num_threads;
    p->bytes  = bytes;
    p->seed   = rand();

    pthread_create(&threads[i], NULL, worker_thread, p);
  }

  for(int i = 0; i < num_threads; i++) {
    pthread_join(threads[i], NULL);
  }

  fwrite(bytes, 1, s, stdout);
  free(bytes);
}
