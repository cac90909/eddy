from django.db import models
from django.contrib.postgres.fields import ArrayField

class UserData(models.Model):
    date = models.DateField()
    functionalities = ArrayField(models.TextField(), blank=True, null=True)
    subject_matters = ArrayField(models.TextField(), blank=True, null=True)
    general_categories = ArrayField(models.TextField(), blank=True, null=True)
    title = models.CharField(max_length=255, null=True, blank=True)
    text = models.TextField(null=True, blank=True)
    tags = ArrayField(models.TextField(), blank=True, null=True)
    parents_ids = ArrayField(models.TextField(), blank=True, null=True)
    children_ids = ArrayField(models.TextField(), blank=True, null=True)
    siblings_ids = ArrayField(models.TextField(), blank=True, null=True)
    fields = models.JSONField(null=True, blank=True)
    entry_id = models.CharField(max_length=50, unique=True)
    user_id = models.IntegerField()

    class Meta:
        db_table = 'user_data_test'