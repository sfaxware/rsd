PACKAGE_NAME=rsd
DOC_FORMAT=html
INPUT_LIST=src.lst
DESCR_LIST=dscr.lst

DOC_DIR=../../../build/share/doc/${PACKAGE_NAME}

XML_FILES=$(shell cat ${INPUT_LIST})
PAS_FILES=$(shell cat ${DESCR_LIST})


all:developer_reference

clean:
	${RM} -r ${DOC_DIR}/*

${DOC_DIR}/%.tex:%.tex
	cp $< $@

${DOC_DIR}/%_fpdoc_output.tex:${INPUT_LIST} ${DESCR_LIST} ${XML_FILES} ${PAS_FILES}
	fpdoc --format=latex --package="${PACKAGE_NAME}_ide" --output="$@" --descr=@${DESCR_LIST} --input=@${INPUT_LIST}

${DOC_DIR}%.dvi: ${DOC_DIR}%.tex ${DOC_DIR}/%_fpdoc_output.tex
	cd ${DOC_DIR} && latex -8bit $(notdir $<)

${DOC_DIR}/%.pdf:${DOC_DIR}/%.dvi
	cd ${DOC_DIR} && dvipdf $(notdir $<)

developer_reference:${DOC_DIR}/developer_reference.${DOC_FORMAT}

.PHONY:developer_reference
