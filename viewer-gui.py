#!/usr/bin/env python

from viewer import viewer

import numpy as np
from sdl2 import *
import sdl2.ext
import time
import sys
import imageio
import png
import ctypes as ct

def load_slide(f):
    img = np.array(imageio.imread(f))[:768,:1024]
    colors = np.ndarray((img.shape[0], img.shape[1]), dtype=np.int32)
    colors[:,:]  = np.uint32(img[:,:,0]) << 16
    colors[:,:] += np.uint32(img[:,:,1]) << 8
    colors[:,:] += np.uint32(img[:,:,2]) << 0
    return colors

slides = [ load_slide(f) for f in sys.argv[1:] ]

viewer = viewer(interactive=1)

(height, width) = slides[0].shape
SDL_Init(SDL_INIT_EVERYTHING)
window = SDL_CreateWindow("Futhark Presents!",
                          SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
		          width, height, SDL_WINDOW_SHOWN)

def reWindow(window):
    window_surface = SDL_GetWindowSurface(window)
    frame_py = np.ndarray(shape=(height, width), dtype=np.int32, order='C')
    surface = SDL_CreateRGBSurfaceFrom(frame_py.ctypes.data, width, height, 32, width*4,
                                       0xFF0000, 0xFF00, 0xFF, 0x00000000)
    return (window_surface, frame_py, surface)

(window_surface, frame_py, surface) = reWindow(window)

def advance(state):
    state['viewer_state'] = viewer.advance(state['viewer_state'])

def render(window, state):
    frame_fut = viewer.render(state['viewer_state'])
    frame_fut.get(ary=frame_py)

    SDL_BlitSurface(surface, None, window_surface, None)
    SDL_UpdateWindowSurface(window)

def new_state():
    return { 'slide_index': 0,
             'viewer_state': viewer.load_image(slides[0]),
             'sand': False }

desired_fps = 60.0

def change_slide(d, state):
    i = min(len(slides)-1, max(state['slide_index'] + d, 0))
    state['slide_index'] = i
    state['viewer_state'] = viewer.load_image(slides[i])
    state['sand'] = False

def start_nbody(state):
    state['viewer_state'] = viewer.start_nbody(state['viewer_state'])

def shuffle(state):
    state['viewer_state'] = viewer.shuffle(state['viewer_state'], np.random.rand())

def drop(i, state):
    state['viewer_state'] = viewer.drop_pixels(i, state['viewer_state'])

state = new_state()

running=True
last=time.time()
desired_fps=60
i = 0
while running:
    advance(state)
    if state['sand']:
        i += 1
        drop(i, state)
    if ((time.time()-last) > (1.0/desired_fps)):
        render(window, state)
        last=time.time()
    events = sdl2.ext.get_events()
    for event in events:
        if event.type == SDL_QUIT:
            running=False
        if event.type == SDL_KEYDOWN:
            key = event.key.keysym.sym
            if key == SDLK_q:
                running=False
            elif key == SDLK_RIGHT:
                change_slide(1, state)
            elif key == SDLK_LEFT:
                change_slide(-1, state)
            elif key == SDLK_HOME:
                change_slide(-len(slides), state)
            elif key == SDLK_END:
                change_slide(len(slides), state)
            elif key == SDLK_p:
                start_nbody(state)
            elif key == SDLK_r:
                shuffle(state)
            elif key == SDLK_d:
                slide = viewer.render(state['viewer_state']).get()
                state['viewer_state'] = viewer.load_image(slide)
                state['sand']=True
            elif key == SDLK_q:
                sys.exit()
            elif key == SDLK_f:
                SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN_DESKTOP)
                (window_surface, frame_py, surface) = reWindow(window)
            elif key == SDLK_g:
                SDL_SetWindowFullscreen(window, 0)
                (window_surface, frame_py, surface) = reWindow(window)
            elif key == SDLK_o:
                state['viewer_state'] = viewer.revert(state['viewer_state'])
