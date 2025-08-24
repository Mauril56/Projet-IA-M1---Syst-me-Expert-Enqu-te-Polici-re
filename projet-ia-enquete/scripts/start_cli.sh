#!/bin/bash
echo "💻 Lancement de l'interface CLI..."
echo "=================================="

# Vérifier que SWI-Prolog est installé
if ! command -v swipl &> /dev/null; then
    echo "❌ SWI-Prolog n'est pas installé"
    echo "Installation requise: sudo apt-get install swi-prolog"
    exit 1
fi

# Se positionner dans le bon répertoire
cd "$(dirname "$0")/.."  # Remonter au répertoire racine du projet

echo "🚀 Interface CLI démarrée"
echo "📁 Dossier racine: $(pwd)"
echo "⏹️  Pour quitter: tapez '9' dans le menu"
echo "=================================="

# Lancer l'interface CLI
swipl -q -g "use_module('src/interface/cli_interface')" -g "cli_interface:start_cli" -t halt