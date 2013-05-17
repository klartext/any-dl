# Maintainer: oliver < a t >  first . in-berlin . de

pkgname=any-dl
pkgver=0.9.6
pkgrel=1
pkgdesc="Generic video downloader for principially any site."
arch=('i686' 'x86_64')
license=('GPL3')
source=(http://www.first.in-berlin.de/software/tools/any-dl/any-dl-0.9.6.tgz)
md5sums=('6bed39f8a37008a354f1fa4a4b0a62b9')
url="http://www.first.in-berlin.de/software/tools/any-dl/"
depends=('ocaml' 'ocaml-pcre' 'ocaml-xml-light' 'ocamlnet' 'ocaml-curl')
makedepends=('ocaml-findlib')
options=(!makeflags)

build() {
cd ${srcdir}/${pkgname}-${pkgver}
make
}


package() {
cd ${srcdir}/${pkgname}-${pkgver}
install -Dm 644 any-dl ${pkgdir}/usr/bin/any-dl   # install to Arch-Linux path
echo Please copy the file rc-file.adl to $HOME/.any-dl.rc but be sure not to overwrite your possibly exsiting own changes.
}
