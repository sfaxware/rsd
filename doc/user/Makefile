PACKAGE_NAME=rsd
DOC_NAME=user_guide
DOC_FORMAT=html
TOP_DIR=../..
DOC_DIR=${TOP_DIR}/build/share/doc/${PACKAGE_NAME}

XML_FILES=$(wildcard *.xml */*.xml */*/*.xml)
PAS_FILES=$(wildcard $(addprefix ${TOP_DIR}/core/, */*.pas))

INPUT_LIST=${DOC_DIR}/${DOC_NAME}_src.lst
DESCR_LIST=${DOC_DIR}/${DOC_NAME}_dscr.lst

all:user_guide

clean:
	${RM} -r ${DOC_DIR}/*

${INPUT_LIST}:${PAS_FILES}
	echo "$^" | sed -e 's/ /\n/g' > $@

${DESCR_LIST}:${XML_FILES}
	echo "$^" | sed -e 's/ /\n/g' > $@

${DOC_DIR}/%.tex:%.tex
	cp $< $@

${DOC_DIR}/%_fpdoc_output.tex:${INPUT_LIST} ${DESCR_LIST} ${XML_FILES} ${PAS_FILES}
	fpdoc --format=latex --package="${PACKAGE_NAME}_ide" --output="$@" --descr=@${DESCR_LIST} --input=@${INPUT_LIST}

${DOC_DIR}%.toc: ${DOC_DIR}%.tex ${DOC_DIR}/%_fpdoc_output.tex
	cd ${DOC_DIR} && latex -8bit $(notdir $<)

${DOC_DIR}%.dvi: ${DOC_DIR}%.tex ${DOC_DIR}/%_fpdoc_output.tex ${DOC_DIR}%.toc
	cd ${DOC_DIR} && latex -8bit $(notdir $<)

${DOC_DIR}/%.pdf:${DOC_DIR}/%.dvi
	cd ${DOC_DIR} && dvipdf $(notdir $<)

user_guide:${DOC_DIR}/user_guide.${DOC_FORMAT}

.PHONY:user_guide
