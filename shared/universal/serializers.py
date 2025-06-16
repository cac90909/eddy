from rest_framework import serializers
from shared.models import Universal, Snapshots  # adjust the import to your actual model location

class UniversalSerializer(serializers.ModelSerializer):
    class Meta:
        model = Universal
        #fields = '__all__'  # or list the fields you want to expose
        exclude = ('user',)
