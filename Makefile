#=====================================================================
# make site
#=====================================================================

#=====================================================================
# Setup
#=====================================================================
# CORE MACROS
ifeq ($(OS), Windows_NT)
CD=cd
else
CD=cd -P "$(CURDIR)"; cd   # This handles the case when CURDIR is a softlink
endif
CP=cp
MAKE=make
MV=mv
RM=rm -f
MKDIR=mkdir -p
RMDIR=$(RM) -r
ASPELL=aspell
SORT=sort
R_SCRIPT = Rscript

# Capabilities
HAS_ASPELL := $(shell $(R_SCRIPT) -e "cat(Sys.getenv('HAS_ASPELL', !is.na(utils:::aspell_find_program('aspell'))))")
HTML_FILES := $(wildcard html/*.html html/*/*.html html/*/*/*.html)


#=====================================================================
# Configs
#=====================================================================


#=====================================================================
# Global
#=====================================================================
all: build


#=====================================================================
# Scrape, old build etc.
#=====================================================================
scrape:
	$(R_SCRIPT) "R/scrape-old.R"

build_scrape:
	$(R_SCRIPT) "R/build15.R" --input=scrape

build_content_tmp:
	$(MKDIR) content,tmp/
	rsync -avvz scraped/5.rsp/ content,tmp/
	rsync -avvz --checksum content/ content,tmp/

build_both: build_content_tmp
	$(R_SCRIPT) "R/build15.R" --input=content,tmp


#=====================================================================
# Build site
#=====================================================================
assets/references/references.bib:
	$(R_SCRIPT) "R/build_references.R"

assets/ico/favicon.png:
	$(CD) assets/ico/; \
	$(R_SCRIPT) "favicon.R"

references: assets/references/references.bib

favicon: assets/ico/favicon.png

build_content:
	$(R_SCRIPT) "R/build15.R" --input=content

build: favicon references build_content


#=====================================================================
# Lists
#=====================================================================
images: images.log

images.log:
	grep -i -E "[.](png|gif|jpg)" html/*.html > images1.log
	grep -i -E "[.](png|gif|jpg)" html/*/*.html >> images1.log
	grep -i -E "[.](png|gif|jpg)" html/*/*/*.html >> images1.log
	sed -E "s/\.(png|gif|jpg).*/.\1/g;" images1.log > images2.log
	sed -E "s/.*(src|href)=['\"]//g;" images2.log > images3.log
	sed -E "s/.*assets\//assets\//g;" images3.log > images4.log
	sed "s/%2B/+/g;" images4.log > images5.log
	sed "s/%2C/,/g;" images5.log > images6.log
	sed "s/%28/(/g;" images6.log > images7.log
	sed "s/%29/)/g;" images7.log > images8.log
	sort -u images8.log > images.log
	$(RM) images?.log
	cat images.log

copy_images: images.log
	for file in `cat images.log | grep assets/`; do \
	  $(MKDIR) images/`dirname $$file`; \
	  $(CP) $$file images/`dirname $$file`; \
	done


#=====================================================================
# Checks
#=====================================================================
spell:
	@echo $(HTML_FILES)
	$(RM) spell-words.txt
	for file in $(HTML_FILES); do \
	  echo file=$$file; \
          $(ASPELL) --personal=./.aspell.en_US.pws --mode=html list < $$file >> spell-words.txt; \
	done
	cat spell-words.txt | sort -u > spell-words.sorted.txt
	cat spell-words.sorted.txt

check_images: images.log
	echo "Missing image file(s):" > images.missing.log
	for file in `cat images.log`; do \
	  if ! test -f $$file; then \
	    echo $$file >> images.missing.log; \
	  fi \
	done
	cat images.missing.log	

check_links:
	wget -R '*+url+*' --spider -o wget.log -e robots=off -w 1 -r -p http://www.aroma-project.org/index.html
	grep -B 2 '404 Not Found' wget.log > wget.404.log
	cat wget.404.log

check_html:
	$(R_SCRIPT) "R/w3c-html"

check_css:
	$(R_SCRIPT) -e "browseURL('http://jigsaw.w3.org/css-validator/validator?uri=http%3A%2F%2Fwww.aroma-project.org')"



#=====================================================================
# Publish (=go live!)
#=====================================================================
publish:
	rsync -avvz --exclude '*~' --exclude 'references/*' --perms --chmod=ugo+rx --progress html/ aroma-project.org:public_html/aroma-project.org


#=====================================================================
# Cleanups
#=====================================================================
clean:
	$(RM) -rf html/
