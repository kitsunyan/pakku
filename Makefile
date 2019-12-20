VERSION = 0.14
COPYRIGHT = 2018-2019 kitsunyan
DIST_MODE = false

MAN_PAGES = \
	doc/pakku.8 \
	doc/pakku.conf.5

TARGETS =  \
	completion/bash \
	completion/zsh \
	lib/tools \
	src/pakku \
	${MAN_PAGES}

TARGETS_NODIST = \
	${MAN_PAGES:=.in}

DIST = \
	COPYING \
	Makefile \
	pakku.conf \
	completion/bash.patch \
	completion/bash-git.patch \
	completion/make.sh \
	completion/zsh.patch \
	completion/zsh-git.patch \
	doc/asciidoc.conf \
	${MAN_PAGES:=.txt} \
	$(shell find src lib -type f -name '*.nim')

EXTRA_DIST = \
	${MAN_PAGES:=.in}

DESTDIR =
PREFIX = /usr/local

BINDIR = ${PREFIX}/bin
PKGLIBDIR = ${PREFIX}/lib/pakku
BASHCOMPLETIONSDIR = ${PREFIX}/share/bash-completion/completions
ZSHCOMPLETIONSDIR = ${PREFIX}/share/zsh/site-functions
MANDIR = ${PREFIX}/share/man
LOCALSTATEDIR = /var
SYSCONFDIR = /etc

ifneq ($(wildcard .git),)
RVERSION = $(shell (git describe --tags 2> /dev/null || echo v${VERSION}) | \
tail -c +2 | head -1)
else
RVERSION = ${VERSION}
endif

NIM_TARGET = release
NIM_OPTIMIZE = size
NIM_CACHE_DIR = nimcache

NIM_OPTIONS = \
	--putenv:'PROG_VERSION'="${RVERSION}" \
	--putenv:'PROG_COPYRIGHT'="${COPYRIGHT}" \
	--putenv:'PROG_PKGLIBDIR'="${PKGLIBDIR}" \
	--putenv:'PROG_LOCALSTATEDIR'="${LOCALSTATEDIR}" \
	--putenv:'PROG_SYSCONFDIR'="${SYSCONFDIR}" \
	-d:'${NIM_TARGET}' \
	--opt:'${NIM_OPTIMIZE}' \
	--hint'[Conf]':off \
	--hint'[Processing]':off \
	--hint'[Link]':off \
	--hint'[SuccessX]':off

ASCIIDOC_OPTIONS = \
	-f doc/asciidoc.conf \
	-a manmanual='Pakku Manual' \
	-a mansource='Pakku' \
	-a manversion="${RVERSION}"

.PHONY: all clean install uninstall distcheck

all: \
	${TARGETS} \
	${TARGETS_NODIST}

completion/bash: \
	completion/make.sh \
	completion/bash.patch \
	completion/bash-git.patch
	@echo "GEN: $@"
	@(cd completion && ./make.sh 'bash')

completion/zsh: \
	completion/make.sh \
	completion/zsh.patch \
	completion/zsh-git.patch
	@echo "GEN: $@"
	@(cd completion && ./make.sh 'zsh')

${MAN_PAGES:=.in}: ${MAN_PAGES:=.txt}
	@echo "GEN: $@"
	@a2x \
	--doctype manpage \
	--format manpage \
	--asciidoc-opts="${ASCIIDOC_OPTIONS}" \
	"${@:.in=}.txt"
	@mv "${@:.in=}" "$@"

${MAN_PAGES}: ${MAN_PAGES:=.in}
	@echo "GEN: $@"
	@sed \
	-e 's,{sysconfdir},${SYSCONFDIR},' \
	-e '/^[\.'"'"']\\"/d' \
	< "${@:=.in}" > "$@"

lib/tools: lib/tools.nim $(shell find lib -name \*.nim)
	@echo "NIM: $@"
	@nim c ${NIM_OPTIONS} \
	--nimcache:"${NIM_CACHE_DIR}/tools" \
	-o:"$@" "$<"

src/pakku: src/main.nim $(shell find src -name \*.nim)
	@echo "NIM: $@"
	@nim c ${NIM_OPTIONS} \
	--nimcache:"${NIM_CACHE_DIR}/main" \
	-o:"$@" "$<"

clean:
	@rm -rfv nimcache
	@rm -rfv ${TARGETS}
ifneq (${DIST_MODE},true)
	@rm -rfv ${TARGETS_NODIST}
endif

define install
	@echo 'INSTALL: $3'
	@install -Dm$1 $2 '${DESTDIR}$3'
endef

define uninstall
	@echo 'UNINSTALL: $1/$2'
	@rm '${DESTDIR}$1/$2'
	@rmdir -p '${DESTDIR}$1' 2> /dev/null || true
endef

install:
	$(call install,644,'completion/bash','${BASHCOMPLETIONSDIR}/pakku')
	$(call install,644,'completion/zsh','${ZSHCOMPLETIONSDIR}/_pakku')
	$(call install,644,'doc/pakku.8','${MANDIR}/man8/pakku.8')
	$(call install,644,'doc/pakku.conf.5','${MANDIR}/man5/pakku.conf.5')
	$(call install,755,'lib/tools','${PKGLIBDIR}/tools')
	@echo 'INSTALL: ${PKGLIBDIR}/bisect'
	@ln -s tools ${DESTDIR}${PKGLIBDIR}/bisect
	@echo 'INSTALL: ${PKGLIBDIR}/install'
	@ln -s tools ${DESTDIR}${PKGLIBDIR}/install
	$(call install,755,'src/pakku','${BINDIR}/pakku')
	$(call install,644,'pakku.conf','${SYSCONFDIR}/pakku.conf')

uninstall:
	$(call uninstall,'${BASHCOMPLETIONSDIR}','pakku')
	$(call uninstall,'${ZSHCOMPLETIONSDIR}','_pakku')
	$(call uninstall,'${MANDIR}/man8','pakku.8')
	$(call uninstall,'${MANDIR}/man5','pakku.conf.5')
	$(call uninstall,'${PKGLIBDIR}','tools')
	$(call uninstall,'${PKGLIBDIR}','bisect')
	$(call uninstall,'${PKGLIBDIR}','install')
	$(call uninstall,'${BINDIR}','pakku')
	$(call uninstall,'${SYSCONFDIR}','pakku.conf')

distcheck:
	@rm -rf 'pakku-${RVERSION}'
	@mkdir 'pakku-${RVERSION}'
	@for f in ${DIST}; do cp -d --parents $$f 'pakku-${RVERSION}'; done

	@sed -i 'pakku-${RVERSION}/Makefile' \
	-e 's/^VERSION =.*/VERSION = ${RVERSION}/' \
	-e 's/^DIST_MODE =.*/DIST_MODE = true/'

	@(cd 'pakku-${RVERSION}' && \
	make && \
	mkdir 'destdir' && \
	make DESTDIR="`pwd`/destdir" install && \
	make DESTDIR="`pwd`/destdir" uninstall && \
	[ ! -d 'destdir' ] && \
	make clean)

	@tar -cJvf 'pakku-${RVERSION}.tar.xz' \
	${DIST:%='pakku-${RVERSION}'/%} \
	${EXTRA_DIST:%='pakku-${RVERSION}'/%}

	@rm -rf 'pakku-${RVERSION}'
