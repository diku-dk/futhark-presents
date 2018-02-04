#!/usr/bin/env python

from viewer import viewer

import numpy
import pygame
import time
import sys
import imageio
import png

def load_slide(f):
    return numpy.array(imageio.imread(f))

def compute_background(img):
    colors = numpy.ndarray((img.shape[0], img.shape[1]), dtype=numpy.uint32)
    colors[:,:]  = numpy.uint32(img[:,:,0]) << 16
    colors[:,:] += numpy.uint32(img[:,:,1]) << 8
    colors[:,:] += numpy.uint32(img[:,:,2]) << 0
    (values,counts) = numpy.unique(colors,return_counts=True)
    return values[numpy.argmax(counts)]

slides = map(load_slide, sys.argv[1:])

viewer = viewer(interactive=1)

pygame.init()
pygame.display.set_caption('Viewer')
(slide_width, slide_height, _) = slides[0].shape
(height, width) = (slide_width, slide_height)
size = (width, height)
screen = pygame.display.set_mode(size)
surface = pygame.Surface((slide_height, slide_width))

def advance(state):
    state['viewer_state'] = viewer.advance(state['viewer_state'])

def render(state):
    frame = viewer.render(state['viewer_state']).get()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))
    pygame.display.flip()

def new_state():
    return { 'slide_index': 0,
             'viewer_state': viewer.load_image(slides[0], compute_background(slides[0])) }

desired_fps = 60.0

def start_running():
    pygame.time.set_timer(pygame.USEREVENT+1, int(1000.0/desired_fps))

def stop_running():
    pygame.time.set_timer(pygame.USEREVENT+1, 0)

def change_slide(d, state):
    stop_running()
    i = min(len(slides)-1, max(state['slide_index'] + d, 0))
    state['slide_index'] = i
    state['viewer_state'] = viewer.load_image(slides[i], compute_background(slides[i]))

def start_nbody(state):
    # To work around a bug in either Futhark or Beignet, we perform
    # the filtering of white pixels on the CPU.  This is not necessary
    # on NVIDIA GPUs.
    bodies, keep = viewer.bodies_and_flags(state['viewer_state'])
    bodies = bodies.get()
    keep = keep.get()
    bodies = bodies[keep]
    start_running()
    state['viewer_state'] = viewer.start_nbody_prefiltered(state['viewer_state'], bodies)

state = new_state()

while True:
    render(state)
    events = pygame.event.get()
    if len(events) == 0:
        events = [pygame.event.wait()]
    for event in events:
        if event.type == pygame.USEREVENT+1:
            advance(state)
            pygame.event.clear(pygame.USEREVENT+1)
        elif event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_RIGHT:
                change_slide(1, state)
            elif event.key == pygame.K_LEFT:
                change_slide(-1, state)
            if event.key == pygame.K_HOME:
                change_slide(-len(slides), state)
            elif event.key == pygame.K_END:
                change_slide(len(slides), state)
            elif event.key == pygame.K_p:
                start_nbody(state)
            elif event.key == pygame.K_q:
                sys.exit()
            elif event.key == pygame.K_f:
                pygame.display.set_mode(size, pygame.FULLSCREEN)
            elif event.key == pygame.K_g:
                pygame.display.set_mode(size)
            elif event.key == pygame.K_o:
                state['viewer_state'] = viewer.revert(state['viewer_state'])
