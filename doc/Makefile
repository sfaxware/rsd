PACKAGE_NAME=rsd
DOC_FORMAT=html
DOC_DIR=../build/share/doc/${PACKAGE_NAME}

INPUT_LIST=src.lst
DESCR_LIST=dscr.lst

XML_FILES=$(shell cat ${INPUT_LIST})
PAS_FILES=$(shell cat ${DESCR_LIST})

DOC_INDEX=${DOC_DIR}/index.${DOC_FORMAT}

all:${DOC_FORMAT}

clean:
	${RM} -r ${DOC_DIR}/*

%docs:%

${DOC_FORMAT}:${DOC_INDEX}

${DOC_INDEX}:${INPUT_LIST} ${DESCR_LIST} ${XML_FILES} ${PAS_FILES}
	fpdoc --format=${DOC_FORMAT} --package="${PACKAGE_NAME}_ide" --output="${DOC_DIR}" --descr=@${DESCR_LIST} --input=@${INPUT_LIST}
