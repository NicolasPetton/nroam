# Dependencies to install. The project depends on a version of org
# that is more recent than the one provided by Emacs 27.1. Because
# makel will refuse to install org because it is built into Emacs, we
# force a newer version by asking org-plus-contrib instead.
ELPA_DEPENDENCIES=package-lint org-plus-contrib org-roam buttercup f s

ELPA_ARCHIVES=melpa org

TEST_BUTTERCUP_OPTIONS=tests
LINT_CHECKDOC_FILES=$(wildcard *.el)
LINT_PACKAGE_LINT_FILES=nroam.el
LINT_COMPILE_FILES=$(wildcard *.el)

makel.mk:
	# Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://gitlab.petton.fr/DamienCassou/makel/raw/v0.6.0/makel.mk; \
	fi

# Include makel.mk if present
-include makel.mk
