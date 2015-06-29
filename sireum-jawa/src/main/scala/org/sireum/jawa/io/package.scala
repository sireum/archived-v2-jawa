package org.sireum.jawa

/**
 * @author fgwei
 */
package object io {
  type JManifest = java.util.jar.Manifest
  type JFile = java.io.File

  implicit def enrichManifest(m: JManifest): Jar.WManifest = Jar.WManifest(m)
}