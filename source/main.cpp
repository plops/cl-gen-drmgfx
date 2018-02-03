//! \file main.cpp Draw to screen using linux direct rendering manager
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <sys/mman.h>
#include <unistd.h>

#include <xf86drm.h>
#include <xf86drmMode.h>
//! This repository contains a minimal program to draw to a linux screen.
//! \section Dependencies
//! - Linux kernel with DRM driver
//! - libdrm

//! - sbcl to generate c++ code
//! - g++ to compile c++ code

//! - For the documentation (optional):
//!   + doxygen

//! \section References
//! 1. http://betteros.org/tut/graphics1.php

//! @brief main function
//!
//! @usage draw to screen
//!
//! @param argc input number of command line arguments
//! @param argv input
//!
//! @return Integer

int main(int argc, char **argv) {
  if ((0 == drmAvailable())) {
    (std::cerr << "drm not available" << std::endl);
  }
  {
    int fd = drmOpen("i915", nullptr);
    if ((fd < 0)) {
      (std::cerr << "drmOpen error: " << fd << std::endl);
    }
    {
      auto r = drmSetClientCap(fd, DRM_CLIENT_CAP_UNIVERSAL_PLANES, 1);
      if ((0 != r)) {
        (std::cerr << "drmSetClientCap error: " << r << std::endl);
      }
    }
    {
      auto res = drmModeGetResources(fd);
      assert(res);
      {
        drmModeConnectorPtr c = nullptr;
        for (unsigned int i = 0;
             (i < static_cast<unsigned int>(res->count_connectors)); i += 1) {
          c = drmModeGetConnector(fd, res->connectors[i]);
          assert(c);
          if ((DRM_MODE_CONNECTED == c->connection)) {
            break;
          }
          drmFree(c);
        }
        {
          auto enc = drmModeGetEncoder(fd, c->encoder_id);
          auto crtc = drmModeGetCrtc(fd, enc->crtc_id);
          auto fb = drmModeGetFB(fd, crtc->buffer_id);
          auto plane_res = drmModeGetPlaneResources(fd);
          auto plane = ({
            drmModePlanePtr p = nullptr;
            for (unsigned int i = 0; (i < plane_res->count_planes); i += 1) {
              p = drmModeGetPlane(fd, plane_res->planes[i]);
              assert(p);
              if ((p->fb_id == fb->fb_id)) {
                break;
              }
              drmFree(p);
            }
            p;
          });
          assert(enc);
          assert(crtc);
          assert(fb);
          assert(plane_res);
          {
            uint64_t has_dumb = 0;
            assert((!(drmGetCap(fd, DRM_CAP_DUMB_BUFFER, &has_dumb))));
            assert(has_dumb);
          }
          {
            struct drm_mode_create_dumb creq;
            memset(&creq, 0, sizeof(creq));
            creq.width = fb->width;
            creq.height = fb->height;
            creq.bpp = fb->bpp;
            assert((!(drmIoctl(fd, DRM_IOCTL_MODE_CREATE_DUMB, &creq))));
            (std::cout << "width=" << creq.width << " height=" << creq.height
                       << std::endl);
            {
              uint32_t my_fb = 0;
              assert((!(drmModeAddFB(fd, creq.width, creq.height, 24, creq.bpp,
                                     creq.pitch, creq.handle, &my_fb))));
              {
                struct drm_mode_map_dumb mreq;
                memset(&mreq, 0, sizeof(mreq));
                mreq.handle = creq.handle;
                assert((!(drmIoctl(fd, DRM_IOCTL_MODE_MAP_DUMB, &mreq))));
                {
                  uint32_t *map = static_cast<uint32_t *>(
                      mmap(0, creq.size, (PROT_READ | PROT_WRITE), MAP_SHARED,
                           fd, mreq.offset));
                  assert((MAP_FAILED != map));
                  memset(map, 0, creq.size);
                  for (unsigned int i = 0; (i < 256); i += 1) {
                    for (unsigned int j = 0; (j < 256); j += 1) {
                      map[(j + (i * (creq.pitch >> 2)))] = 0x12345678;
                    }
                  }
                  assert((!(drmModeSetCrtc(fd, crtc->crtc_id, my_fb, 0, 0,
                                           &c->connector_id, 1, &crtc->mode))));
                  sleep(10);
                  assert((!(drmModeSetCrtc(fd, crtc->crtc_id, fb->fb_id, 0, 0,
                                           &c->connector_id, 1, &crtc->mode))));
                  assert((!(drmModeRmFB(fd, my_fb))));
                  {
                    struct drm_mode_destroy_dumb dreq;
                    memset(&dreq, 0, sizeof(dreq));
                    dreq.handle = creq.handle;
                    assert(
                        (!(drmIoctl(fd, DRM_IOCTL_MODE_DESTROY_DUMB, &dreq))));
                  }
                  drmFree(plane);
                  drmFree(plane_res);
                  drmFree(fb);
                  drmFree(crtc);
                  drmFree(enc);
                  drmFree(c);
                  drmFree(res);
                  drmClose(fd);
                }
              }
            }
          }
        }
      }
    }
  }
  return 0;
}