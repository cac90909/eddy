from django.db import models
from django.contrib.postgres.fields import ArrayField

from django.contrib.auth import get_user_model

User = get_user_model()

class Universal(models.Model):
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
    user = models.ForeignKey(User, on_delete=models.CASCADE)

    class Meta:
        db_table = 'universal'

class Snapshot(models.Model):
    snapshot_id = models.AutoField(primary_key=True)
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name="snapshots")
    title = models.CharField(max_length=255)
    description = models.CharField(max_length=255)
    operation_chain = models.JSONField(default=list, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        db_table = 'snapshots'