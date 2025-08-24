#!/bin/bash
echo "🚀 Lancement de l'interface CLI..."
echo "================================="

cd src/
swipl -q -s core/enquete_core.pl -g main -t halt

echo ""
echo "================================="
echo "👋 Programme terminé"