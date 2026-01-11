#include <limits.h>
// #include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define STBI_NO_FAILURE_STRINGS
#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION
#define STBI_FAILURE_USERMSG
#include "stb_image.h"
#include "stb_image_write.h"

#include "seam_carving.h"

image* image_load(char* filename) {
    int w, h, channels;
    uint8_t* data = stbi_load(filename, &w, &h, &channels, 0);
    if (!data) {
        fprintf(stderr, "Erreur de lecture.\n");
        stbi_failure_reason();
        exit(EXIT_FAILURE);
    }
    if (channels != 1) {
        fprintf(stdout, "Pas une image en niveaux de gris.\n");
        exit(EXIT_FAILURE);
    }
    image* im = image_new(h, w);
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            im->at[i][j] = data[j + i * w];
        }
    }
    free(data);
    return im;
}

void image_save(image* im, char* filename) {
    int h = im->h;
    int w = im->w;
    int stride_length = w;
    uint8_t* data = malloc(w * h * sizeof(uint8_t));
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            data[j + i * w] = im->at[i][j];
        }
    }
    if (!stbi_write_png(filename, w, h, 1, data, stride_length)) {
        fprintf(stderr, "Erreur d'écriture.\n");
        image_free(im);
        free(data);
        exit(EXIT_FAILURE);
    }
    free(data);
}

image* image_new(int h, int w) {
    uint8_t** lines = malloc(sizeof(uint8_t*) * h);
    for (int i = 0; i < h; i++) {
        uint8_t* column = malloc(sizeof(uint8_t) * w);
        lines[i] = column;
    }
    image* img = malloc(sizeof(image));
    img->at = lines;
    img->h = h;
    img->w = w;
    return img;
}

void image_free(image* im) {
    for (int line_idx = 0; line_idx < im->h; line_idx++) {
        free(im->at[line_idx]);
    }
    free(im->at);
    free(im);
}

void invert(image* im) {
    for (int line_idx = 0; line_idx < im->h; line_idx++) {
        for (int column_idx = 0; column_idx < im->w; column_idx++) {
            uint8_t* pixel = &im->at[line_idx][column_idx];
            *pixel = UINT8_MAX-*pixel;
        }
    }
}

void binarize(image* im) {
    uint8_t threshold = UINT8_MAX/2;
    for (int line_idx = 0; line_idx < im->h; line_idx++) {
        for (int column_idx = 0; column_idx < im->w; column_idx++) {
            uint8_t* pixel = &im->at[line_idx][column_idx];
            *pixel = *pixel <= threshold ? 0 : UINT8_MAX;
        }
    }
}

void flip_horizontal(image* im) {
    for (int line_idx = 0; line_idx < im->h; line_idx++) {
        uint8_t* line = im->at[line_idx];
        for (int column_idx = 0; column_idx < im->w/2; column_idx++) {
            uint8_t temp = line[column_idx];
            line[column_idx] = line[im->w - 1 -column_idx];
            line[im->w - 1 -column_idx] = temp;
        }
    }
}

energy* energy_new(int h, int w) {
    double** lines = malloc(sizeof(uint8_t*) * h);
    for (int i = 0; i < h; i++) {
        double* column = malloc(sizeof(double) * w);
        lines[i] = column;
    }
    energy* en = malloc(sizeof(image));
    en->at = lines;
    en->h = h;
    en->w = w;
    return en;
}

void energy_free(energy* e) {
    for (int line_idx = 0; line_idx < e->h; line_idx++) {
        free(e->at[line_idx]);
    }
    free(e->at);
    free(e);
}

int min(int a, int b) { return a > b ? b : a; }
int max(int a, int b) { return a < b ? b : a; }
double abs_d(double a) { return a < 0 ? -a : a; }

void compute_energy(image* im, energy* e) {
    for (int line_idx = 0; line_idx < im->h; line_idx++) {
        for (int column_idx = 0; column_idx < im->w; column_idx++) {
            int i = line_idx;
            int j = column_idx;
            int ib = min(i + 1, im->h - 1);
            int jr = min(j + 1, im->w - 1);
            int it = max(i - 1, 0);
            int jl = max(j - 1, 0);
            double energy = 
                (abs_d((double)im->at[i][jr] - (double)im->at[i][jl]) / (double)(jr - jl))
                + (abs_d((double)im->at[ib][j] - (double)im->at[it][j]) / (double)(ib - it));
            e->at[i][j] = energy;
        }
    }
}

image* energy_to_image(energy* e) {
    image* im = image_new(e->h, e->w);
    for (int line = 0; line < im->h; line++) {
        for (int column = 0; column < im->w; column++) {
            double energy = e->at[line][column];
            im->at[line][column] = energy;
        }
    }
    return im;
}

void remove_pixel(uint8_t* line, double* e, int w) {
    int min_energy_idx = 0;
    for (int i = 1; i < w; i++)
        if (e[min_energy_idx] > e[i]) min_energy_idx = i;

    for (int i = min_energy_idx; i < w-1; i++) {
        uint8_t temp = line[i];
        line[i] = line[i+1];
        line[i+1] = temp;
    }
}

void reduce_one_pixel(image* im, energy* e);

void reduce_pixels(image* im, int n);

int best_column(energy* e);

void reduce_one_column(image* im, energy* e);

void reduce_column(image* im, int n);

void energy_min_path(energy* e);

path* path_new(int n);

void path_delete(path* p);

void compute_min_path(energy* e, path* p);

void reduce_seam_carving(image* im, int n);

int main(int argc, char* argv[]) {
    if (argc < 3) {
        printf("Fournir le fichier d'entrée et de sortie.\n");
        exit(EXIT_FAILURE);
    }
    char* f_in = argv[1];
    char* f_out = argv[2];
    image* im = image_load(f_in);

    // Do some processing here
    // invert(im);
    // binarize(im);
    // flip_horizontal(im);

    energy* e = energy_new(im->h, im->w);
    compute_energy(im, e);

    for (int l = 0; l < im->h; l++) {
        for (int o = im->w; o > 100; o--) {
            remove_pixel(im->at[l], e->at[l], o);
        }
        // remove_pixel(im->at[l], e->at[l], im->w);
        // remove_pixel(im->at[l], e->at[l], im->w-1);
        // remove_pixel(im->at[l], e->at[l], im->w-2);
        // remove_pixel(im->at[l], e->at[l], im->w-3);
        // remove_pixel(im->at[l], e->at[l], im->w-4);
        // remove_pixel(im->at[l], e->at[l], im->w-5);
        // remove_pixel(im->at[l], e->at[l], im->w-6);
    }
    // image* energy_image = energy_to_image(e);
    
    image_save(im, f_out);
    image_free(im);
    // image_free(energy_image);
    energy_free(e);
    return 0;
}
