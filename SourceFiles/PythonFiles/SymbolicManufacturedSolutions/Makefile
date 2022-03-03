# Makefile for python codes
#
# 

VENV = venv
PYTHON = $(VENV)/bin/python3
PIP = $(VENV)/bin/pip

run: $(VENV)/bin/activate
	 $(PYTHON) main.py


$(VENV)/bin/activate: requirements.txt
	 python3 -m venv $(VENV)
	  $(PIP) install -r requirements.txt

test:
	python -m unittest tests/test_helpers.py

clean:
	 rm -rf __pycache__
	 rm -rf $(VENV)
	 rm *.log
