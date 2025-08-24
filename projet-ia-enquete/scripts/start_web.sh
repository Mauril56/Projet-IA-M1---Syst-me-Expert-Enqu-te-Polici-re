#!/bin/bash
echo "🌐 Lancement de l'interface web..."
echo "=================================="

# Vérifier que Python est installé
if ! command -v python3 &> /dev/null; then
    echo "❌ Python3 n'est pas installé"
    echo "Installation requise: sudo apt-get install python3"
    exit 1
fi

# Se positionner dans le bon répertoire
cd "$(dirname "$0")/.."  # Remonter au répertoire racine du projet

echo "🚀 Serveur web démarré sur http://localhost:8000"
echo "📁 Dossier web: $(pwd)/web"
echo "⏹️  Pour arrêter: Ctrl+C"
echo "=================================="

# Lancer le serveur web Python depuis le répertoire web
cd web
python3 -m http.server 8000