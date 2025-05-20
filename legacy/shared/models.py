from django.db import models
from django.contrib.auth import get_user_model
from django.contrib.postgres.fields import ArrayField

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


class Snapshots(models.Model):
    # Using the default primary key field renamed as dataset_id
    snapshot_id = models.AutoField(primary_key=True)

    # Foreign key to the User model.
    # Can access user snapshot data by user.snapshots.ACCESSOR -> ex: "user.snapshots.all()"
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name="snapshots")

    # A title that the user assigns to the dataset.
    title = models.CharField(max_length=255)

    # Additional info explaining the nature of the data
    description = models.CharField(max_length=255)

    # Stores the chain of operations that produced the dataset.
    # Using a JSONField lets you naturally store a list (or dict) of operations.
    operation_chain = models.JSONField(default=list, blank=True)

    # Optional: Timestamps to track creation and updates.
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        db_table = 'snapshots'