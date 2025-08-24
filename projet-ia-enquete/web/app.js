// Simulation des données Prolog
const knowledgeBase = {
    suspects: ['john', 'mary', 'alice', 'bruno', 'sophie'],
    crimes: ['vol', 'assassinat', 'escroquerie'],
    
    // Règles de culpabilité
    isGuilty: function(suspect, crime) {
        switch (crime) {
            case 'vol':
                return suspect === 'john';
            case 'assassinat':
                return suspect === 'mary';
            case 'escroquerie':
                return suspect === 'alice';
            default:
                return false;
        }
    },
    
    // Génération de rapport
    generateReport: function() {
        let report = `RAPPORT D'ENQUÊTE - ${new Date().toLocaleDateString()}\n\n`;
        
        report += "SUSPECTS INVESTIGUÉS:\n";
        this.suspects.forEach(suspect => {
            report += `\n${suspect.toUpperCase()}:\n`;
            this.crimes.forEach(crime => {
                const guilty = this.isGuilty(suspect, crime);
                report += `  - ${crime}: ${guilty ? 'COUPABLE' : 'INNOCENT'}\n`;
            });
        });
        
        return report;
    },
    
    // Statistiques
    getStats: function() {
        const stats = {
            totalSuspects: this.suspects.length,
            totalCrimes: this.crimes.length,
            guiltyCount: 0
        };
        
        this.suspects.forEach(suspect => {
            this.crimes.forEach(crime => {
                if (this.isGuilty(suspect, crime)) {
                    stats.guiltyCount++;
                }
            });
        });
        
        return stats;
    }
};

function checkGuilt() {
    const suspect = document.getElementById('suspectSelect').value;
    const crime = document.getElementById('crimeSelect').value;
    const resultDiv = document.getElementById('result');
    
    const isGuilty = knowledgeBase.isGuilty(suspect, crime);
    
    resultDiv.className = isGuilty ? 'result guilty' : 'result innocent';
    resultDiv.innerHTML = `
        <strong>Résultat:</strong> ${suspect} est 
        <strong>${isGuilty ? 'COUPABLE' : 'INNOCENT'}</strong> 
        pour ${crime}
    `;
}

function generateReport() {
    const report = knowledgeBase.generateReport();
    document.getElementById('report').textContent = report;
    
    // Afficher les statistiques
    const stats = knowledgeBase.getStats();
    const statsDiv = document.getElementById('stats');
    
    statsDiv.innerHTML = `
        <div class="stat-item">
            <h3>${stats.totalSuspects}</h3>
            <p>Suspects</p>
        </div>
        <div class="stat-item">
            <h3>${stats.totalCrimes}</h3>
            <p>Types de crimes</p>
        </div>
        <div class="stat-item">
            <h3>${stats.guiltyCount}</h3>
            <p>Coupables identifiés</p>
        </div>
    `;
}

// Initialisation
document.addEventListener('DOMContentLoaded', function() {
    generateReport();
});