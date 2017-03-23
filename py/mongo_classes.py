import os, sys, time, resource, re, gc, shutil
from multiprocess import Pool
from functools import partial
from mongoengine import *
from urllib.parse import urlparse, parse_qsl
connect('mongoengine_documents')


class scopus_doc(DynamicDocument):
    scopus_id = StringField(required=True, max_length=50, unique=True)
    PY = IntField(required=True)

class scopus_ref(Document):
    text = StringField(required=True, unique=True)
    ti = StringField()
    PY = IntField()
    extra = StringField()
    doi = StringField()
    url = URLField()
